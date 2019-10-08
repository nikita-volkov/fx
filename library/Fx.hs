module Fx
(
  -- * IO
  fx,
  -- * Fx
  Fx,
  liftSafeIO,
  start,
  wait,
  concurrently,
  -- * Future
  Future,
  -- * Conc
  Conc,
  sequentially,
  -- * Classes
  FxLifting(..),
  Failing(..),
)
where

import Fx.Prelude hiding (app)
import qualified Data.Text as Text
import qualified Data.HashSet as HashSet


-- * IO
-------------------------

{-|
Execute an effect with all errors handled.

Conventionally, this is what should be placed in the @main@ function.
-}
fx :: Fx Void a -> IO a
fx (Fx m) = do

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
    env = Env crash childCountVar childTidsVar
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
Effectful computation with explicit errors.

Calling `fail` causes it to interrupt,
killing all of its threads and outputting a message to console.
`fail` is intended to be used in events which you expect never to happen,
and hence which should be considered bugs.
It is similar to calling `fail` on IO,
with a major difference of the error never getting lost in a concurrent environment.
-}
newtype Fx err a = Fx (ReaderT Env (ExceptT err IO) a)

data Env = Env (String -> IO ()) (TVar Int) (TVar (HashSet ThreadId))

deriving instance Functor (Fx err)
deriving instance Applicative (Fx err)
deriving instance Monoid err => Alternative (Fx err)
deriving instance Monad (Fx err)
deriving instance MonadFix (Fx err)
deriving instance MonadError err (Fx err)
deriving instance Monoid err => MonadPlus (Fx err)
deriving instance Apply (Fx err)
deriving instance Bind (Fx err)
deriving instance Semigroup err => Alt (Fx err)
deriving instance Monoid err => Plus (Fx err)

instance MonadFail (Fx err) where
  fail = Fx . liftIO . fail

mapFx fn (Fx m) = Fx (fn m)

{-|
Turn IO action into a non-failing action.
It is your responsibility to ensure that it does not throw exceptions.
-}
liftSafeIO :: IO a -> Fx err a
liftSafeIO io = Fx (liftIO io)

start :: Fx err a -> Fx err' (Future err a)
start (Fx m) =
  Fx $ ReaderT $ \ (Env crash childCountVar childTidsVar) -> ExceptT $ do

    futureVar <- newEmptyMVar

    atomically (modifyTVar' childCountVar succ)

    forkIO $ do

      tid <- myThreadId

      atomically (modifyTVar' childTidsVar (HashSet.insert tid))

      finalize <- catch
        (do
          res <- runExceptT (runReaderT m (Env crash childCountVar childTidsVar))
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

wait :: Future err a -> Fx err a
wait (Future fx) = fx

concurrently :: Conc err a -> Fx err a
concurrently (Conc fx) = fx


-- * Future
-------------------------

{-|
Handle to a result of an action which may still be being calculated.

The way you deal with it is thru the `start` and `wait` functions.
-}
newtype Future err a =
  {-| A blocking action, producing a result or failing. -}
  Future (Fx err a)
  deriving (Functor, Applicative, Monad, MonadError err)

mapFuture fn (Future m) = Future (fn m)


-- * Conc
-------------------------

{-|
Wrapper over `Fx`,
whose instances compose by running computations on separate threads.

You can turn `Fx` into `Conc` using `sequentially`.
-}
newtype Conc err a = Conc (Fx err a)

deriving instance Functor (Conc err)

instance Applicative (Conc err) where
  pure = Conc . pure
  (<*>) (Conc m1) (Conc m2) = Conc $ do
    future1 <- start m1
    res2 <- m2
    res1 <- wait future1
    return (res1 res2)

mapConc fn (Conc m) = Conc (fn m)

sequentially :: Fx err a -> Conc err a
sequentially = Conc


-- * Classes
-------------------------

-- ** Fx Lifting
-------------------------

{-|
Support for lifting of `Fx`.

Apart from other things this is your interface to turn `Fx` into `IO` or `Conc`.
-}
class FxLifting err m | m -> err where
  liftFx :: Fx err a -> m a

instance FxLifting err (ExceptT err IO) where
  liftFx fx = ExceptT (liftFx (exposeErr fx))

instance FxLifting Void IO where
  liftFx = fx

instance FxLifting err (Fx err) where
  liftFx = id

instance FxLifting err (Conc err) where
  liftFx = Conc

instance FxLifting err (Future err) where
  liftFx = Future

-- ** Failing
-------------------------

class Failing m where

  {-|
  Expose the error in result,
  producing an action, which is compatible with any error type.

  This function is particularly helpful, when you need to map into error of type `Void`.
  -}
  exposeErr :: m a res -> m b (Either a res)

  {-|
  Map from error to result, leaving the error be anything.

  This function is particularly helpful, when you need to map into error of type `Void`.
  -}
  absorbErr :: (a -> res) -> m a res -> m b res

  {-|
  Handle error in another failing action.

  This function is particularly helpful, when you need to map into error of type `Void`.
  -}
  bindErr :: (a -> m b res) -> m a res -> m b res

instance Failing Fx where
  exposeErr = mapFx $ mapReaderT $ mapExceptT $ fmap $ Right
  absorbErr errProj = mapFx $ mapReaderT $ mapExceptT $ fmap $ either (Right . errProj) Right
  bindErr handler = mapFx $ \ m -> ReaderT $ \ unmask -> ExceptT $ do
    a <- runExceptT (runReaderT m unmask)
    case a of
      Right res -> return (Right res)
      Left err -> case handler err of
        Fx m -> runExceptT (runReaderT m unmask)

instance Failing Future where
  exposeErr = mapFuture exposeErr
  absorbErr fn = mapFuture (absorbErr fn)
  bindErr fn (Future m) = Future (bindErr (fn >>> \ (Future m') -> m') m)

instance Failing Conc where
  exposeErr = mapConc exposeErr
  absorbErr fn = mapConc (absorbErr fn)
  bindErr fn (Conc m) = Conc (bindErr (fn >>> \ (Conc m') -> m') m)
