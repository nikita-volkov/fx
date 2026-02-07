module Fx
  ( -- * Fx
    Fx,

    -- ** Execution
    RunsFx (..),

    -- ** Environment handling
    scoping,
    mapEnv,

    -- ** Error handling
    throwErr,
    handleErr,
    mapErr,
    exposeErr,
    absorbErr,

    -- ** Concurrency
    concurrently,

    -- ** IO execution

    -- |
    -- These functions leak abstraction in one way or the other,
    -- requiring you to ensure that your code doesn't throw unexpected exceptions.
    -- `try` and `catch` are your tools for that.
    --
    -- Besides these functions `Fx` also has an instance of `MonadIO`,
    -- which provides the only non-leaky way of running IO, catching all possible exceptions.
    runTotalIO,
    runPartialIO,
    runExceptionalIO,

    -- * Scope
    Scope,
    acquire,
    releasing,

    -- * Exceptions
    FxException (..),
    FxExceptionReason (..),
  )
where

import Fx.Prelude hiding (app)
import qualified Fx.Strings as Strings

-- * IO

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

-- * Fx

-------------------------

-- |
-- Effectful computation with explicit errors in the context of provided environment.
--
-- Think of it as a combination of `ReaderT` and `ExceptT` over `IO`, with some extra features for concurrency and error handling.
--
-- __Key Properties__:
--
-- - Designed for explicit dependency injection and error propagation.
-- - In concurrent settings, errors are raised as exceptions to avoid silent failures.
-- - `fail` and `error` never get lost in a concurrent setting.
--
-- __Usage__: Use for any effectful logic, parametric over context and failures. Compose with `mapEnv`/`mapErr` for modularity.
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
  throwError = throwErr
  catchError = flip handleErr

instance Bifunctor (Fx env) where
  bimap lf rf = mapFx (mapReaderT (mapExceptT (fmap (bimap lf rf))))

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

mapFx ::
  ( ReaderT (FxEnv env1) (ExceptT err1 IO) res1 ->
    ReaderT (FxEnv env2) (ExceptT err2 IO) res2
  ) ->
  Fx env1 err1 res1 ->
  Fx env2 err2 res2
mapFx fn (Fx m) = Fx (fn m)

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

-- * Future

-------------------------

-- |
-- Handle to a result of an action which may still be being executed on another thread.
--
-- The way you deal with it is thru the `start` and `wait` functions.
newtype Future err res = Future (Compose STM (Either (Maybe err)) res)
  deriving (Functor, Applicative, Alternative)

instance Bifunctor Future where
  bimap lf rf = mapFuture (mapCompose (fmap (bimap (fmap lf) rf)))

mapFuture ::
  ( Compose STM (Either (Maybe err1)) res1 ->
    Compose STM (Either (Maybe err2)) res2
  ) ->
  Future err1 res1 ->
  Future err2 res2
mapFuture fn (Future m) = Future (fn m)

-- |
-- Spawn a thread and start running an effect on it,
-- returning the associated future.
--
-- Fatal errors on the spawned thread are guaranteed to get propagated to the top.
-- By fatal errors we mean calls to `error`, `fail` and uncaught exceptions.
--
-- Normal errors (the explicit @err@ parameter) will only propagate
-- if you use `wait` at some point.
--
-- __Warning:__
-- It is your responsibility to ensure that the whole future executes
-- before the running `Fx` finishes.
-- Otherwise you will lose the environment in scope of which the future executes.
-- To achieve that use `wait`.
start :: Fx env err res -> Fx env err' (Future err res)
start (Fx m) =
  Fx $
    ReaderT $
      \(FxEnv unmask crash env) -> lift $ do
        futureVar <- newEmptyTMVarIO

        _ <- forkIO $ do
          tid <- myThreadId

          let childCrash tids dls = crash (tid : tids) dls

          finalize <-
            catch
              ( do
                  res <- runExceptT (runReaderT m (FxEnv unmask childCrash env))
                  return (atomically (putTMVar futureVar (first Just res)))
              )
              ( \exc -> return $ do
                  case fromException exc of
                    -- Catch calls to `error`.
                    Just errorCall -> crash [] (ErrorCallFxExceptionReason errorCall)
                    -- Catch anything else we could miss. Just in case.
                    _ -> crash [] (BugFxExceptionReason (Strings.unexpectedException exc))
                  atomically (putTMVar futureVar (Left Nothing))
              )

          finalize

        return $ Future $ Compose $ readTMVar futureVar

-- |
-- Block until the future completes either with a result or an error.
wait :: Future err res -> Fx env err res
wait (Future m) = Fx $
  ReaderT $
    \(FxEnv unmask crash _) ->
      ExceptT $
        join $
          catch
            ( do
                futureStatus <- unmask (atomically (getCompose m))
                return $ case futureStatus of
                  Right res -> return (Right res)
                  Left (Just err) -> return (Left err)
                  Left Nothing -> fail "Waiting for a future that crashed"
            )
            ( \(exc :: SomeException) -> return $ do
                crash [] (BugFxExceptionReason (Strings.failedWaitingForResult exc))
                fail "Thread crashed with uncaught exception waiting for result."
            )

-- |
-- Execute concurrent effects by composing them applicatively.
--
-- E.g.,
--
-- > selectDataById :: Int64 -> Fx env err (Metadata, File)
-- > selectDataById id =
-- >   concurrently $ \lift ->
-- >     (,)
-- >       <$> lift (selectMetadataById id)
-- >       <*> lift (getFileById id)
concurrently ::
  (forall f. (Alternative f) => (forall x. Fx env err x -> f x) -> f res) ->
  Fx env err res
concurrently build =
  case build Conc of
    Conc fx -> fx

-- * Conc

-------------------------

-- |
-- Wrapper over `Fx`,
-- whose instances compose by running computations on separate threads.
--
-- You can turn `Fx` into `Conc` using `runFx`.
newtype Conc env err res = Conc (Fx env err res)

deriving instance Functor (Conc env err)

deriving instance Bifunctor (Conc env)

instance Applicative (Conc env err) where
  pure = Conc . pure
  (<*>) (Conc m1) (Conc m2) = Conc $ do
    future1 <- start m1
    res2 <- m2
    res1 <- wait future1
    return (res1 res2)

instance Alternative (Conc env err) where
  empty = Conc do
    wait empty
  (<|>) (Conc m1) (Conc m2) = Conc $ do
    future1 <- start m1
    future2 <- start m2
    wait (future1 <|> future2)

-- * Scope

-------------------------

-- |
-- An applicative computation that acquires an environment `env` and may fail with `err`. Ideal for resource management.
--
-- __Key Properties__:
--
-- - Composes multiple acquisitions (e.g., open DB + S3).
-- - Ensures release actions are called, even on errors.
-- - Acts as an `Applicative` and `Functor`.
--
-- __Example__:
--
-- > postgres :: Scope PostgresErr Connection
-- > postgres = releasing Postgres.disconnect $
-- >   acquire (Postgres.connect "postgres://...")
-- >
-- > redis :: Scope RedisErr Redis
-- > redis = releasing Redis.disconnect $
-- >   acquire (Redis.connect "redis://...")
-- >
-- > appEnv :: Scope AppErr AppEnv
-- > appEnv = AppEnv <$> first PostgresErr postgres <*> first RedisErr redis
--
-- __Usage__: Wrap resource providers; use `with` to scope `Fx` computations within acquired resources.
--
-- Builds up on ideas expressed in http://www.haskellforall.com/2013/06/the-resource-applicative.html
-- and later released as the [\"managed\"](https://hackage.haskell.org/package/managed) package.
newtype Scope err env = Scope (Fx () err (env, Fx () err ()))

instance Functor (Scope err) where
  fmap f (Scope m) = Scope $ do
    (env, release) <- m
    return (f env, release)

instance Applicative (Scope err) where
  pure env = Scope (pure (env, pure ()))
  Scope m1 <*> Scope m2 =
    Scope $
      liftA2 (\(env1, release1) (env2, release2) -> (env1 env2, release2 *> release1)) m1 m2

instance Bifunctor Scope where
  bimap lf rf (Scope m) = Scope (bimap lf (bimap rf (first lf)) m)
  second = fmap

-- |
-- Create a resource provider from an acquiring effect.
--
-- To add a release action, use 'releasing'.
acquire :: Fx () err env -> Scope err env
acquire acquire = Scope do
  env <- acquire
  return (env, pure ())

-- |
-- Add a release action to an existing resource provider.
--
-- Example:
--
-- > fileInWriteMode :: FilePath -> Scope IOError Handle
-- > fileInWriteMode path =
-- >   releasing hClose (acquire (openFile path WriteMode))
releasing :: Fx env err () -> Scope err env -> Scope err env
releasing release (Scope m) = Scope $ do
  (env, existingRelease) <- m
  return (env, existingRelease >> closeEnv env release)

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

-- * Exceptions

-------------------------

-- |
-- Fatal failure of an `Fx` application.
-- Informs of an unrecoverable condition that the application has reached.
-- It is not meant to be caught,
-- because it implies that there is either a bug in your code or
-- a bug in the "fx" library itself, which needs reporting.
--
-- Consists of a list of thread identifiers specifying the nesting path of
-- the faulty thread and the reason of failure.
data FxException = FxException [ThreadId] FxExceptionReason

instance Show FxException where
  show (FxException tids reason) = Strings.fatalErrorAtThreadPath tids (show reason)

instance Exception FxException

-- |
-- Reason of a fatal failure of an `Fx` application.
data FxExceptionReason
  = UncaughtExceptionFxExceptionReason SomeException
  | ErrorCallFxExceptionReason ErrorCall
  | BugFxExceptionReason String

instance Show FxExceptionReason where
  show = \case
    UncaughtExceptionFxExceptionReason exc -> Strings.uncaughtException exc
    ErrorCallFxExceptionReason errorCall -> show errorCall
    BugFxExceptionReason details -> Strings.bug details
