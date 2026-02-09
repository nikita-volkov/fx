-- |
-- Core Fx operations and execution
module Fx.Internal.Fx
  ( -- * Execution
    runFxInIO,
    RunsFx (..),

    -- ** Environment handling
    scoping,
    mapEnv,
    closeEnv,

    -- ** Error handling
    throwErr,
    handleErr,
    mapErr,
    exposeErr,
    absorbErr,

    -- ** IO execution
    runTotalIO,
    runPartialIO,
    runExceptionalIO,
  )
where

import Fx.Prelude
import qualified Fx.Strings as Strings
import Fx.Internal.Types

-- * IO Execution

-------------------------

-- |
-- Execute an effect with no environment and all errors handled.
--
-- Conventionally, this is what should be placed in the @main@ function.
runFxInIO :: Fx () Void res -> IO res
runFxInIO (Fx m) = uninterruptibleMask $ \unmask -> do
  fatalErrChan <- newTQueueIO
  resVar <- newEmptyTMVarIO

  _ <- forkIO $ do
    tid <- myThreadId

    finalize <-
      let crash tids reason = atomically (writeTQueue fatalErrChan (FxException (tid : tids) reason))
          fxEnv = FxEnv unmask crash ()
       in catch
            ( do
                resOrVoid <- runExceptT (runReaderT m fxEnv)
                return $ case resOrVoid of
                  Right res -> atomically (putTMVar resVar res)
                  Left a ->
                    catch
                      ( do
                          _ <- evaluate a
                          crash [] (BugFxExceptionReason "Unexpected void")
                      )
                      (crash [] . ErrorCallFxExceptionReason)
            )
            ( \exc -> return $ case fromException exc of
                -- Catch calls to `error`.
                Just errorCall -> crash [] (ErrorCallFxExceptionReason errorCall)
                -- Catch anything else we could miss. Just in case.
                _ -> crash [] (BugFxExceptionReason (Strings.unexpectedException exc))
            )

    -- Throw errors or post the result
    finalize

  -- Wait for fatal error or result
  join $
    catch
      ( unmask $
          atomically $
            asum
              [ do
                  fatalErr <- readTQueue fatalErrChan
                  return $ throwIO fatalErr,
                do
                  res <- readTMVar resVar
                  return $ return res
              ]
      )
      ( \(exc :: SomeException) ->
          case fromException exc of
            Just (exc :: AsyncException) -> throwIO exc
            _ -> throwIO (FxException [] (BugFxExceptionReason (Strings.failedWaitingForFinalResult exc)))
      )

-- |
-- Turn a non-failing IO action into an effect.
--
-- __Warning:__
-- It is your responsibility to ensure that it does not throw exceptions!
runTotalIO :: (env -> IO res) -> Fx env err res
runTotalIO io = Fx $
  ReaderT $
    \(FxEnv unmask crash env) ->
      lift $
        catch
          (unmask (io env))
          ( \(exc :: SomeException) -> do
              crash [] (UncaughtExceptionFxExceptionReason exc)
              fail "Unhandled exception in runTotalIO. Got propagated to top."
          )

-- |
-- Run IO which produces either an error or result.
--
-- __Warning:__
-- It is your responsibility to ensure that it does not throw exceptions!
runPartialIO :: (env -> IO (Either err res)) -> Fx env err res
runPartialIO io = runTotalIO io >>= either throwErr return

-- |
-- Run IO which only throws a specific type of exception.
--
-- __Warning:__
-- It is your responsibility to ensure that it doesn't throw any other exceptions!
runExceptionalIO :: (Exception exc) => (env -> IO res) -> Fx env exc res
runExceptionalIO io =
  Fx $
    ReaderT $
      \(FxEnv unmask crash env) -> ExceptT $
        catch (fmap Right (unmask (io env))) $
          \exc -> case fromException exc of
            Just exc' -> return (Left exc')
            Nothing -> do
              crash [] (UncaughtExceptionFxExceptionReason exc)
              fail "Unhandled exception in runExceptionalIO. Got propagated to top."

-- |
-- Execute Fx in the scope of a provided environment.
scoping :: Scope err env -> Fx env err res -> Fx env' err res
scoping (Scope (Fx acquire)) (Fx fx) =
  Fx $
    ReaderT $
      \(FxEnv unmask crash _) -> ExceptT $ do
        let providerFxEnv = FxEnv unmask crash ()
        acquisition <- runExceptT (runReaderT acquire providerFxEnv)
        case acquisition of
          Left err -> return (Left err)
          Right (env, (Fx release)) -> do
            resOrErr <- runExceptT (runReaderT fx (FxEnv unmask crash env))
            releasing <- runExceptT (runReaderT release providerFxEnv)
            return (resOrErr <* releasing)

closeEnv :: env -> Fx env err res -> Fx env' err res
closeEnv env (Fx fx) =
  Fx $
    ReaderT $
      \(FxEnv unmask crash _) ->
        ExceptT $
          runExceptT (runReaderT fx (FxEnv unmask crash env))

-- |
-- Map the environment.
-- Please notice that the expected function is contravariant.
mapEnv :: (b -> a) -> Fx a err res -> Fx b err res
mapEnv fn (Fx m) =
  Fx $
    ReaderT $
      \(FxEnv unmask crash env) ->
        runReaderT m (FxEnv unmask crash (fn env))

-- * Classes

-------------------------

-- ** Fx Running

-------------------------

-- |
-- Support for running of `Fx`.
--
-- Apart from other things this is your interface to turn `Fx` into `IO`.
class RunsFx env err m | m -> env, m -> err where
  runFx :: Fx env err res -> m res

-- |
-- Executes an effect with no environment and all errors handled.
instance RunsFx () Void IO where
  runFx = runFxInIO

instance RunsFx () err (ExceptT err IO) where
  runFx fx = ExceptT (runFx (exposeErr fx))

instance RunsFx env err (ReaderT env (ExceptT err IO)) where
  runFx fx = ReaderT (\env -> ExceptT (runFx (mapEnv (const env) (exposeErr fx))))

instance RunsFx env err (Fx env err) where
  runFx = id

instance RunsFx env err (Conc env err) where
  runFx = Conc

instance RunsFx () err (Scope err) where
  runFx fx = Scope (fmap (\env -> (env, pure ())) fx)

-- ** Error Handling

-------------------------

-- |
-- Interrupt the current computation raising an error.
--
-- Same as `throwError` of `MonadError`.
throwErr :: err -> Fx env err res
throwErr = Fx . lift . throwE

-- |
-- Handle error in another failing action.
-- Sort of like a bind operation over the error type parameter.
--
-- Similar to `handleError` of `MonadError` but allows changing the error type.
handleErr :: (a -> Fx env b res) -> Fx env a res -> Fx env b res
handleErr handler = mapFx $ \m -> ReaderT $ \unmask -> ExceptT $ do
  a <- runExceptT (runReaderT m unmask)
  case a of
    Right res -> return (Right res)
    Left err -> case handler err of
      Fx m -> runExceptT (runReaderT m unmask)

-- |
-- Map the error.
mapErr :: (a -> b) -> Fx env a res -> Fx env b res
mapErr mapper = handleErr (throwErr . mapper)

-- |
-- Expose the error in result,
-- producing an action, which is compatible with any error type.
--
-- Almost the same as `tryError` of `MonadError`, but allows changing the error type.
exposeErr :: Fx env a res -> Fx env b (Either a res)
exposeErr = absorbErr Left . fmap Right

-- |
-- Map from error to result, leaving the error be anything.
absorbErr :: (a -> res) -> Fx env a res -> Fx env b res
absorbErr fn = handleErr (pure . fn)
