module Fx
  ( -- * Fx
    Fx,

    -- ** Execution
    FxRunning (..),
    runFxHandling,

    -- ** Error handling
    ErrHandling (..),
    mapErr,
    exposeErr,
    absorbErr,

    -- ** Environment handling
    EnvMapping (..),
    provideAndUse,
    exposeEnv,

    -- ** Concurrency
    start,
    wait,
    concurrently,

    -- ** IO execution

    -- |
    -- These functions leak abstraction in one way or the other,
    -- requiring you to ensure that your code doesn't throw unexpected exceptions.
    -- `try` are `catch` are your tools for that.
    --
    -- Besides these functions `Fx` also has an instance of `MonadIO`,
    -- which provides the only non-leaky way of running IO, catching all possible exceptions.
    runTotalIO,
    runPartialIO,
    runExceptionalIO,
    runSTM,

    -- * Provider
    Provider,
    acquireAndRelease,
    pool,

    -- * Future
    Future,

    -- * Conc
    Conc,

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
  join
    $ catch
      ( unmask
          $ atomically
          $ asum
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
-- Calling `fail` causes the whole app to interrupt outputting a message to console.
-- `fail` is intended to be used in events which you expect never to happen,
-- and hence which should be considered bugs.
-- It is similar to calling `fail` on IO,
-- with a major difference of the error never getting lost in a concurrent environment.
--
-- Calling `fail` results in `ErrorCallFxExceptionReason` in the triggerred `FxException`.
-- Thus in effect it is the same as calling the `error` function.
newtype Fx env err res = Fx (ReaderT (FxEnv env) (ExceptT err IO) res)

deriving instance Functor (Fx env err)

deriving instance Applicative (Fx env err)

deriving instance (Monoid err) => Alternative (Fx env err)

deriving instance Monad (Fx env err)

deriving instance (Monoid err) => MonadPlus (Fx env err)

instance MonadFail (Fx env err) where
  fail msg = Fx
    $ ReaderT
    $ \(FxEnv _ crash _) -> liftIO $ do
      crash [] (ErrorCallFxExceptionReason (ErrorCall msg))
      fail "Crashed"

instance MonadIO (Fx env SomeException) where
  liftIO io = Fx (ReaderT (\(FxEnv unmask _ _) -> ExceptT (try (unmask io))))

instance Bifunctor (Fx env) where
  bimap lf rf = mapFx (mapReaderT (mapExceptT (fmap (bimap lf rf))))

-- |
-- Runtime and application environment.
data FxEnv env = FxEnv (forall a. IO a -> IO a) ([ThreadId] -> FxExceptionReason -> IO ()) env

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
runTotalIO io = Fx
  $ ReaderT
  $ \(FxEnv unmask crash env) ->
    lift
      $ catch
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
  Fx
    $ ReaderT
    $ \(FxEnv unmask crash env) -> ExceptT
      $ catch (fmap Right (unmask (io env)))
      $ \exc -> case fromException exc of
        Just exc' -> return (Left exc')
        Nothing -> do
          crash [] (UncaughtExceptionFxExceptionReason exc)
          fail "Unhandled exception in runExceptionalIO. Got propagated to top."

-- |
-- Run STM, crashing in case of STM exceptions.
--
-- Same as @`runTotalIO` . `atomically`@.
runSTM :: (env -> STM res) -> Fx env err res
runSTM stm = runTotalIO (atomically . stm)

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
  Fx
    $ ReaderT
    $ \(FxEnv unmask crash env) -> lift $ do
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
wait (Future m) = Fx
  $ ReaderT
  $ \(FxEnv unmask crash _) ->
    ExceptT
      $ join
      $ catch
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
-- Execute concurrent effects.
concurrently :: Conc env err res -> Fx env err res
concurrently (Conc fx) = fx

-- |
-- Execute Fx in the scope of a provided environment.
provideAndUse :: Provider err env -> Fx env err res -> Fx env' err res
provideAndUse (Provider (Fx acquire)) (Fx fx) =
  Fx
    $ ReaderT
    $ \(FxEnv unmask crash _) -> ExceptT $ do
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
  Fx
    $ ReaderT
    $ \(FxEnv unmask crash _) ->
      ExceptT
        $ runExceptT (runReaderT fx (FxEnv unmask crash env))

-- |
-- Expose the environment.
exposeEnv :: Fx env err env
exposeEnv = Fx $ ReaderT $ \(FxEnv _ _ env) -> return env

-- * Future

-------------------------

-- |
-- Handle to a result of an action which may still be being executed on another thread.
--
-- The way you deal with it is thru the `start` and `wait` functions.
newtype Future err res = Future (Compose STM (Either (Maybe err)) res)
  deriving (Functor, Applicative)

instance Bifunctor Future where
  bimap lf rf = mapFuture (mapCompose (fmap (bimap (fmap lf) rf)))

mapFuture ::
  ( Compose STM (Either (Maybe err1)) res1 ->
    Compose STM (Either (Maybe err2)) res2
  ) ->
  Future err1 res1 ->
  Future err2 res2
mapFuture fn (Future m) = Future (fn m)

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

-- * Provider

-------------------------

-- |
-- Effectful computation with explicit errors,
-- which encompasses environment acquisition and releasing.
--
-- Composes well, allowing you to merge multiple providers into one.
--
-- Builds up on ideas expressed in http://www.haskellforall.com/2013/06/the-resource-applicative.html
-- and later released as the \"managed\" package.
newtype Provider err env = Provider (Fx () err (env, Fx () err ()))

instance Functor (Provider err) where
  fmap f (Provider m) = Provider $ do
    (env, release) <- m
    return (f env, release)

instance Applicative (Provider err) where
  pure env = Provider (pure (env, pure ()))
  Provider m1 <*> Provider m2 =
    Provider
      $ liftA2 (\(env1, release1) (env2, release2) -> (env1 env2, release2 *> release1)) m1 m2

instance Monad (Provider err) where
  return = pure
  (>>=) (Provider m1) k2 = Provider $ do
    (env1, release1) <- m1
    (env2, release2) <- case k2 env1 of Provider m2 -> m2
    return (env2, release2 >> release1)

instance MonadIO (Provider SomeException) where
  liftIO = runFx . liftIO

instance Bifunctor Provider where
  bimap lf rf (Provider m) = Provider (bimap lf (bimap rf (first lf)) m)
  second = fmap

-- |
-- Create a resource provider from acquiring and releasing effects.
acquireAndRelease :: Fx () err env -> Fx env err () -> Provider err env
acquireAndRelease acquire release = Provider $ do
  env <- acquire
  return (env, closeEnv env release)

-- |
-- Convert a single resource provider into a pool provider.
--
-- The wrapper provider acquires the specified amount of resources using the original provider,
-- and returns a modified version of the original provider,
-- whose acquisition and releasing merely takes one resource out of the pool and puts it back when done.
-- No actual acquisition or releasing happens in the wrapped provider.
-- No errors get raised in it either.
--
-- Use this when you need to access a resource concurrently.
pool :: Int -> Provider err env -> Provider err (Provider err' env)
pool poolSize (Provider acquire) = Provider $ do
  queue <- runSTM (const newTQueue)
  replicateM_ poolSize $ do
    handle <- acquire
    runSTM $ const $ writeTQueue queue handle
  let resourceProvider = Provider $ do
        (env, releaseResource) <- runSTM $ const $ readTQueue queue
        return (env, runSTM (const (writeTQueue queue (env, releaseResource))))
      release = do
        releasers <- runSTM
          $ const
          $ do
            list <- flushTQueue queue
            guard (length list == poolSize)
            return (fmap snd list)
        sequence_ releasers
   in return (resourceProvider, release)

-- * Classes

-------------------------

-- ** Fx Running

-------------------------

-- |
-- Support for running of `Fx`.
--
-- Apart from other things this is your interface to turn `Fx` into `IO` or `Conc`.
class FxRunning env err fx | fx -> env, fx -> err where
  runFx :: Fx env err res -> fx res

-- |
-- Run `Fx` handling its error in the context monad.
runFxHandling :: (Monad m, FxRunning env Void m) => (err -> m a) -> Fx env err a -> m a
runFxHandling handler fx = runFx (exposeErr fx) >>= either handler return

-- |
-- Executes an effect with no environment and all errors handled.
instance FxRunning () Void IO where
  runFx = runFxInIO

instance FxRunning () err (ExceptT err IO) where
  runFx fx = ExceptT (runFx (exposeErr fx))

instance FxRunning env err (ReaderT env (ExceptT err IO)) where
  runFx fx = ReaderT (\env -> ExceptT (runFx (mapEnv (const env) (exposeErr fx))))

-- |
-- Executes an effect with no environment and all errors handled in `Fx`
-- with any environment and error.
--
-- Same as @(`mapEnv` (`const` ()) . `first` `absurd`)@.
instance FxRunning () Void (Fx env err) where
  runFx = mapEnv (const ()) . first absurd

instance FxRunning env err (Conc env err) where
  runFx = Conc

instance FxRunning () err (Provider err) where
  runFx fx = Provider (fmap (\env -> (env, pure ())) fx)

-- ** ErrHandling

-------------------------

-- |
-- Support for error handling.
--
-- Functions provided by this class are particularly helpful,
-- when you need to map into error of type `Void`.
class ErrHandling fx where
  -- |
  --  Interrupt the current computation raising an error.
  throwErr :: err -> fx err res

  -- |
  --  Handle error in another failing action.
  --  Sort of like a bind operation over the error type parameter.
  handleErr :: (a -> fx b res) -> fx a res -> fx b res

-- |
-- Map the error.
mapErr :: (ErrHandling m) => (a -> b) -> m a res -> m b res
mapErr mapper = handleErr (throwErr . mapper)

-- |
-- Expose the error in result,
-- producing an action, which is compatible with any error type.
exposeErr :: (ErrHandling m, Functor (m a), Applicative (m b)) => m a res -> m b (Either a res)
exposeErr = absorbErr Left . fmap Right

-- |
-- Map from error to result, leaving the error be anything.
absorbErr :: (ErrHandling m, Applicative (m b)) => (a -> res) -> m a res -> m b res
absorbErr fn = handleErr (pure . fn)

instance ErrHandling (Fx env) where
  throwErr = Fx . lift . throwE
  handleErr handler = mapFx $ \m -> ReaderT $ \unmask -> ExceptT $ do
    a <- runExceptT (runReaderT m unmask)
    case a of
      Right res -> return (Right res)
      Left err -> case handler err of
        Fx m -> runExceptT (runReaderT m unmask)

instance ErrHandling Future where
  throwErr = Future . Compose . return . Left . Just
  handleErr handler = mapFuture $ \m -> Compose $ do
    a <- getCompose m
    case a of
      Right res -> return (Right res)
      Left b -> case b of
        Just err -> case handler err of
          Future m' -> getCompose m'
        Nothing -> return (Left Nothing)

deriving instance ErrHandling (Conc env)

-- ** Env Mapping

-------------------------

-- |
-- Support for mapping of the environment.
class EnvMapping fx where
  -- |
  --  Map the environment.
  --  Please notice that the expected function is contravariant.
  mapEnv :: (b -> a) -> fx a err res -> fx b err res

instance EnvMapping Fx where
  mapEnv fn (Fx m) =
    Fx
      $ ReaderT
      $ \(FxEnv unmask crash env) ->
        runReaderT m (FxEnv unmask crash (fn env))

deriving instance EnvMapping Conc

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
