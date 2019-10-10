module Fx
(
  -- * Fx
  Fx,
  -- ** Environment handling
  provideAndUse,
  handleEnv,
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
  -- * Provider
  Provider,
  acquireAndRelease,
  -- * Future
  Future,
  -- * Conc
  Conc,
  -- * Classes
  -- ** FxRunning
  FxRunning(..),
  -- ** ErrHandling
  ErrHandling(..),
  exposeErr,
  absorbErr,
  -- ** EnvMapping
  EnvMapping(..),
)
where

import Fx.Prelude hiding (app)
import qualified Data.Text as Text
import qualified Data.HashSet as HashSet


-- * IO
-------------------------

{-|
Execute an effect with no environment and all errors handled.

Conventionally, this is what should be placed in the @main@ function.
-}
runFxInIO :: Fx () Void res -> IO res
runFxInIO (Fx m) = mask $ \ unmask -> do

  errChan <- newTQueueIO
  resVar <- newEmptyTMVarIO
  childCountVar <- newTVarIO 1
  childTidsVar <- newTVarIO HashSet.empty

  let
    crash msg = do
      childTids <- atomically $ do
        writeTQueue errChan msg
        readTVar childTidsVar
      forM_ (HashSet.toList childTids) killThread
    fxEnv = FxEnv unmask crash childCountVar childTidsVar ()
    in
      forkIO $ do
        catch
          (do
            res <- fmap (either absurd id) (runExceptT (runReaderT m fxEnv))
            atomically (putTMVar resVar res)
          )
          (\ (se :: SomeException) -> crash (fromString (show se)))
        atomically (modifyTVar' childCountVar pred)

  -- Block until all subthreads are dead
  atomically $ do
    childCount <- readTVar childCountVar
    guard (childCount == 0)

  join $ atomically $ do
    asum
      [
        do
          err <- readTQueue errChan
          return $ fail err
        ,
        do
          res <- readTMVar resVar
          return $ return res
      ]


-- * Fx
-------------------------

{-|
Effectful computation with explicit errors in the context of provided environment.

Calling `fail` causes it to interrupt,
killing all of its threads and outputting a message to console.
`fail` is intended to be used in events which you expect never to happen,
and hence which should be considered bugs.
It is similar to calling `fail` on IO,
with a major difference of the error never getting lost in a concurrent environment.
-}
newtype Fx env err res = Fx (ReaderT (FxEnv env) (ExceptT err IO) res)

{-|
Runtime and application environment.
-}
data FxEnv env = FxEnv (forall a. IO a -> IO a) (String -> IO ()) (TVar Int) (TVar (HashSet ThreadId)) env

deriving instance Functor (Fx env err)
deriving instance Applicative (Fx env err)
deriving instance Monoid err => Alternative (Fx env err)
deriving instance Monad (Fx env err)
deriving instance Monoid err => MonadPlus (Fx env err)

instance MonadFail (Fx env err) where
  fail = Fx . liftIO . fail

instance MonadIO (Fx env SomeException) where
  liftIO = Fx . lift . ExceptT . try

instance Bifunctor (Fx env) where
  bimap lf rf = mapFx (mapReaderT (mapExceptT (fmap (bimap lf rf))))

mapFx fn (Fx m) = Fx (fn m)

{-|
Turn a non-failing IO action into an effect.

__Warning:__
It is your responsibility to ensure that it does not throw exceptions!
-}
runTotalIO :: IO res -> Fx env err res
runTotalIO = Fx . liftIO

{-|
Run IO which produces either an error or result.

__Warning:__
It is your responsibility to ensure that it does not throw exceptions!
-}
runPartialIO :: IO (Either err res) -> Fx env err res
runPartialIO = Fx . lift . ExceptT

{-|
Run IO which only throws a specific type of exception.

__Warning:__
It is your responsibility to ensure that it doesn't throw any other exceptions!
-}
runExceptionalIO :: Exception exc => IO res -> Fx env exc res
runExceptionalIO = Fx . lift . ExceptT . try

{-|
Spawn a thread and start running an effect on it,
returning the associated future.
-}
start :: Fx env err res -> Fx env err' (Future env err res)
start (Fx m) =
  Fx $ ReaderT $ \ (FxEnv unmask crash childCountVar childTidsVar env) -> ExceptT $ do

    futureVar <- newEmptyMVar

    atomically (modifyTVar' childCountVar succ)

    forkIO $ do

      tid <- myThreadId

      atomically (modifyTVar' childTidsVar (HashSet.insert tid))

      finalize <- catch
        (do
          res <- runExceptT (runReaderT m (FxEnv unmask crash childCountVar childTidsVar env))
          putMVar futureVar res
          return (return ())
        )
        (\ se -> case fromException se of
          Just ThreadKilled -> return (return ())
          _ -> return (crash (show se))
        )

      -- Deregister
      atomically $ do
        modifyTVar' childCountVar pred
        modifyTVar' childTidsVar (HashSet.delete tid)

      -- Do whatever needs to be done before exiting
      finalize

    return (Right (Future (Fx (lift (ExceptT (readMVar futureVar))))))

{-|
Block until a future completes either with a result or an error.
-}
wait :: Future env err res -> Fx env err res
wait (Future fx) = fx

{-|
Execute concurrent effects.
-}
concurrently :: Conc env err res -> Fx env err res
concurrently (Conc fx) = fx

{-|
Execute Fx in the scope of a provided environment.
-}
provideAndUse :: Provider err env -> Fx env err res -> Fx env' err res
provideAndUse (Provider (Fx acquire)) (Fx fx) =
  Fx $ ReaderT $ \ (FxEnv unmask crash childCountVar childTidsVar _) -> ExceptT $ do
    let providerFxEnv = FxEnv unmask crash childCountVar childTidsVar ()
    acquisition <- runExceptT (runReaderT acquire providerFxEnv)
    case acquisition of
      Left err -> return (Left err)
      Right (env, (Fx release)) -> do
        resOrErr <- runExceptT (runReaderT fx (FxEnv unmask crash childCountVar childTidsVar env))
        releasing <- runExceptT (runReaderT release providerFxEnv)
        return (resOrErr <* releasing)

{-|
Collapse an env handler into an environmental effect.

__Warning:__
This function leaks the abstraction over the environment.
It is your responsibility to ensure that you don't use it to return
the environment and use it outside of the handler's scope.
-}
handleEnv :: (env -> Fx () err res) -> Fx env err res
handleEnv handler =
  Fx $ ReaderT $ \ (FxEnv unmask crash childCountVar childTidsVar env) ->
    case handler env of
      Fx rdr -> runReaderT rdr (FxEnv unmask crash childCountVar childTidsVar ())


-- * Future
-------------------------

{-|
Handle to a result of an action which may still be being calculated.

The way you deal with it is thru the `start` and `wait` functions.
-}
newtype Future env err res =
  {-| A blocking action, producing a result or failing. -}
  Future (Fx env err res)
  deriving (Functor, Applicative, Monad, MonadFail, Bifunctor)

mapFuture fn (Future m) = Future (fn m)


-- * Conc
-------------------------

{-|
Wrapper over `Fx`,
whose instances compose by running computations on separate threads.

You can turn `Fx` into `Conc` using `runFx`.
-}
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

mapConc fn (Conc m) = Conc (fn m)


-- * Provider
-------------------------

{-|
Effectful computation with explicit errors,
which encompasses environment acquisition and releasing.

Composes well, allowing you to merge multiple providers into one.

Builds up on ideas expressed in http://www.haskellforall.com/2013/06/the-resource-applicative.html
and later released as the \"managed\" package.
-}
newtype Provider err env = Provider (Fx () err (env, Fx () err ()))

instance Functor (Provider err) where
  fmap f (Provider m) = Provider $ do
    (env, release) <- m
    return (f env, release)

instance Applicative (Provider err) where
  pure env = Provider (pure (env, pure ()))
  Provider m1 <*> Provider m2 = Provider $
    liftA2 (\ (env1, release1) (env2, release2) -> (env1 env2, release2 *> release1)) m1 m2

instance Monad (Provider err) where
  return = pure
  (>>=) (Provider m1) k2 = Provider $ do
    (env1, release1) <- m1
    (env2, release2) <- case k2 env1 of Provider m2 -> m2
    return (env2, release2 >> release1)

instance Bifunctor Provider where
  bimap lf rf (Provider m) = Provider (bimap lf (bimap rf (first lf)) m)
  second = fmap

{-|
Create a resource provider from acquiring and releasing effects.
-}
acquireAndRelease :: Fx () err env -> (env -> Fx () err ()) -> Provider err env
acquireAndRelease acquire release = Provider $ do
  env <- acquire
  return (env, release env)


-- * Classes
-------------------------

-- ** Fx Running
-------------------------

{-|
Support for running of `Fx`.

Apart from other things this is your interface to turn `Fx` into `IO` or `Conc`.
-}
class FxRunning env err m | m -> env, m -> err where
  runFx :: Fx env err res -> m res

{-|
Executes an effect with no environment and all errors handled.
-}
instance FxRunning () Void IO where
  runFx = runFxInIO

instance FxRunning () err (ExceptT err IO) where
  runFx fx = ExceptT (runFx (exposeErr fx))

instance FxRunning env err (ReaderT env (ExceptT err IO)) where
  runFx fx = ReaderT (\ env -> ExceptT (runFx (mapEnv (const env) (exposeErr fx))))

instance FxRunning env err (Fx env err) where
  runFx = id

instance FxRunning env err (Conc env err) where
  runFx = Conc

instance FxRunning env err (Future env err) where
  runFx = Future

instance FxRunning () err (Provider err) where
  runFx fx = Provider (fmap (\ env -> (env, pure ())) fx)

-- ** ErrHandling
-------------------------

{-|
Support for error handling.

Functions provided by this class are particularly helpful,
when you need to map into error of type `Void`.
-}
class ErrHandling m where

  {-|
  Interrupt the current computation raising an error.
  -}
  throwErr :: err -> m err res

  {-|
  Handle error in another failing action.
  Sort of like a bind operation over the error type parameter.
  -}
  handleErr :: (a -> m b res) -> m a res -> m b res

{-|
Expose the error in result,
producing an action, which is compatible with any error type.
-}
exposeErr :: (ErrHandling m, Functor (m a), Applicative (m b)) => m a res -> m b (Either a res)
exposeErr = absorbErr Left . fmap Right

{-|
Map from error to result, leaving the error be anything.
-}
absorbErr :: (ErrHandling m, Applicative (m b)) => (a -> res) -> m a res -> m b res
absorbErr fn = handleErr (pure . fn)

instance ErrHandling (Fx env) where
  throwErr = Fx . lift . throwE
  handleErr handler = mapFx $ \ m -> ReaderT $ \ unmask -> ExceptT $ do
    a <- runExceptT (runReaderT m unmask)
    case a of
      Right res -> return (Right res)
      Left err -> case handler err of
        Fx m -> runExceptT (runReaderT m unmask)

deriving instance ErrHandling (Future env)
deriving instance ErrHandling (Conc env)

-- ** Env Mapping
-------------------------

{-|
Support for mapping of the environment.
-}
class EnvMapping m where
  {-|
  Map the environment.
  Please notice that the expected function is contravariant.
  -}
  mapEnv :: (b -> a) -> m a err res -> m b err res

instance EnvMapping Fx where
  mapEnv fn (Fx m) =
    Fx $ ReaderT $ \ (FxEnv unmask crash childCountVar childTidsVar env) ->
      runReaderT m (FxEnv unmask crash childCountVar childTidsVar (fn env))

deriving instance EnvMapping Future
deriving instance EnvMapping Conc
