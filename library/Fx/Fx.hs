-- |
-- Core Fx operations and execution
module Fx.Fx
  ( Fx (..),
    FxEnv (..),
    FxException (..),
    FxExceptionReason (..),

    -- * Execution
    runFxInIO,
    RunsFx (..),

    -- ** Environment handling
    mapEnv,
    subtransform,

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

-- * Helper types and instances

-- |
-- Runtime and application environment.
data FxEnv env
  = FxEnv
      -- | Unmasking function.
      (forall a. IO a -> IO a)
      -- | Crash.
      ([ThreadId] -> FxExceptionReason -> IO ())
      -- | User environment.
      env

-- |
-- Fatal failure of an `Fx` application.
data FxException = FxException [ThreadId] FxExceptionReason

-- |
-- Reason of a fatal failure of an `Fx` application.
data FxExceptionReason
  = UncaughtExceptionFxExceptionReason SomeException
  | ErrorCallFxExceptionReason ErrorCall
  | BugFxExceptionReason String

instance Show FxException where
  show (FxException tids reason) = Strings.fatalErrorAtThreadPath tids (show reason)

instance Exception FxException

instance Show FxExceptionReason where
  show = \case
    UncaughtExceptionFxExceptionReason exc -> Strings.uncaughtException exc
    ErrorCallFxExceptionReason errorCall -> show errorCall
    BugFxExceptionReason details -> Strings.bug details

-- * Fx type and instances

-- |
-- Effectful computation with explicit errors in the context of provided environment.
--
-- Think of it as a combination of `ReaderT` and `ExceptT` over `IO`, with some extra features for concurrency and error handling.
newtype Fx env err res = Fx (ReaderT (FxEnv env) (ExceptT err IO) res)

deriving instance Functor (Fx env err)

deriving instance Applicative (Fx env err)

deriving instance (Monoid err) => Alternative (Fx env err)

deriving instance Monad (Fx env err)

deriving instance (Monoid err) => MonadPlus (Fx env err)

instance MonadFail (Fx env err) where
  fail msg = Fx $
    ReaderT $
      \(FxEnv _ crash _) -> liftIO $ do
        crash [] (ErrorCallFxExceptionReason (ErrorCall msg))
        fail "Crashed"

instance MonadIO (Fx env SomeException) where
  liftIO io = Fx (ReaderT (\(FxEnv unmask _ _) -> ExceptT (try (unmask io))))

instance MonadError err (Fx env err) where
  throwError err = Fx (lift (throwE err))
  catchError fx handler = Fx $ ReaderT $ \fxEnv -> ExceptT $ do
    a <- runExceptT (let Fx m = fx in runReaderT m fxEnv)
    case a of
      Right res -> return (Right res)
      Left err -> case handler err of
        Fx m -> runExceptT (runReaderT m fxEnv)

instance Bifunctor (Fx env) where
  bimap lf rf = mapTransformers (mapReaderT (mapExceptT (fmap (bimap lf rf))))

mapTransformers ::
  ( ReaderT (FxEnv env1) (ExceptT err1 IO) res1 ->
    ReaderT (FxEnv env2) (ExceptT err2 IO) res2
  ) ->
  Fx env1 err1 res1 ->
  Fx env2 err2 res2
mapTransformers fn (Fx m) = Fx (fn m)

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
-- Map the environment.
-- Please notice that the expected function is contravariant.
mapEnv :: (b -> a) -> Fx a err res -> Fx b err res
mapEnv fn (Fx m) =
  Fx $
    ReaderT $
      \(FxEnv unmask crash env) ->
        runReaderT m (FxEnv unmask crash (fn env))

-- |
-- Transform an effect by changing its environment and error context, then applying a transformation.
--
-- This is useful for:
--
-- * Running effects with different environments (e.g., focusing on a sub-environment)
-- * Converting between error types while transforming the effect
-- * Applying middleware-like transformations (logging, retries, etc.) in a different context
subtransform ::
  -- | Subenvironment getter.
  (env -> env') ->
  -- | Subenvironment setter.
  (env' -> env -> env) ->
  -- | Suberror packer.
  (err' -> err) ->
  -- | Transformation to apply to the effect in the new context.
  (forall res'. Fx env' err' res' -> Fx env' err' res') ->
  (Fx env err res -> Fx env err res)
subtransform envMap envSet errMap transform fx =
  Fx $ ReaderT $ \(FxEnv unmask crash env) ->
    let env' =
          envMap env
        fx' =
          Fx $ ReaderT $ \(FxEnv _ _ env') ->
            ExceptT $
              let Fx m = fx
                  fxEnv = FxEnv unmask crash (envSet env' env)
               in fmap Right (runExceptT (runReaderT m fxEnv))
        Fx transformedReader =
          transform fx'
     in -- Run the transformed computation in env' and map errors back
        mapExceptT (fmap (first errMap)) (runReaderT transformedReader (FxEnv unmask crash env'))
          >>= either throwE return

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
handleErr handler = mapTransformers $ \m -> ReaderT $ \unmask -> ExceptT $ do
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
