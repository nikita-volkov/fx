module Fx
(
  -- * Fx
  Fx,
  provideAndUse,
  handleEnv,
  start,
  wait,
  concurrently,
  runSafeIO,
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
runFxInIO :: Fx () Void a -> IO a
runFxInIO (Fx m) = do

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
    env = Env crash childCountVar childTidsVar ()
    in
      forkIO $ do
        catch
          (do
            res <- fmap (either absurd id) (runExceptT (runReaderT m env))
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
newtype Fx env err a = Fx (ReaderT (Env env) (ExceptT err IO) a)

{-|
Runtime and application environment.
-}
data Env env = Env (String -> IO ()) (TVar Int) (TVar (HashSet ThreadId)) env

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
Turn an IO action into a non-failing effect.
It is your responsibility to ensure that it does not throw exceptions!
`try`, `catch` from \"base\" and `throwErr` from this library are your tools.
-}
runSafeIO :: IO a -> Fx env err a
runSafeIO io = Fx (liftIO io)

{-|
Spawn a thread and start running an effect on it,
returning the associated future.
-}
start :: Fx env err a -> Fx env err' (Future env err a)
start (Fx m) =
  Fx $ ReaderT $ \ (Env crash childCountVar childTidsVar appEnv) -> ExceptT $ do

    futureVar <- newEmptyMVar

    atomically (modifyTVar' childCountVar succ)

    forkIO $ do

      tid <- myThreadId

      atomically (modifyTVar' childTidsVar (HashSet.insert tid))

      finalize <- catch
        (do
          res <- runExceptT (runReaderT m (Env crash childCountVar childTidsVar appEnv))
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
wait :: Future env err a -> Fx env err a
wait (Future fx) = fx

{-|
Execute concurrent effects.
-}
concurrently :: Conc env err a -> Fx env err a
concurrently (Conc fx) = fx

{-|
Execute Fx in the scope of a provided environment.
-}
provideAndUse :: Provider err env -> Fx env err res -> Fx env' err res
provideAndUse (Provider (Fx acquire)) (Fx fx) =
  Fx $ ReaderT $ \ (Env crash childCountVar childTidsVar _) -> ExceptT $ do
    let providerEnv = Env crash childCountVar childTidsVar ()
    acquisition <- runExceptT (runReaderT acquire providerEnv)
    case acquisition of
      Left err -> return (Left err)
      Right (env, (Fx release)) -> do
        resOrErr <- runExceptT (runReaderT fx (Env crash childCountVar childTidsVar env))
        releasing <- runExceptT (runReaderT release providerEnv)
        return (resOrErr <* releasing)

{-|
Collapse an env handler into an environmental effect.
-}
handleEnv :: (env -> Fx () err res) -> Fx env err res
handleEnv handler =
  Fx $ ReaderT $ \ (Env crash childCountVar childTidsVar env) ->
    case handler env of
      Fx rdr -> runReaderT rdr (Env crash childCountVar childTidsVar ())


-- * Future
-------------------------

{-|
Handle to a result of an action which may still be being calculated.

The way you deal with it is thru the `start` and `wait` functions.
-}
newtype Future env err a =
  {-| A blocking action, producing a result or failing. -}
  Future (Fx env err a)
  deriving (Functor, Applicative, Monad, MonadFail, Bifunctor)

mapFuture fn (Future m) = Future (fn m)


-- * Conc
-------------------------

{-|
Wrapper over `Fx`,
whose instances compose by running computations on separate threads.

You can turn `Fx` into `Conc` using `runFx`.
-}
newtype Conc env err a = Conc (Fx env err a)

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
  runFx :: Fx env err a -> m a

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
    Fx $ ReaderT $ \ (Env crash childCountVar childTidsVar appEnv) ->
      runReaderT m (Env crash childCountVar childTidsVar (fn appEnv))

deriving instance EnvMapping Future
deriving instance EnvMapping Conc
