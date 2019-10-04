module Fx
(
  -- * IO
  app,
  -- * App
  App,
  fx,
  -- * Fx
  Fx,
  start,
  wait,
  concurrently,
  -- * Future
  Future,
  -- * Conc
  Conc,
  sequentially,
)
where

import Fx.Prelude hiding (app)
import qualified Data.Text as Text
import qualified Data.HashSet as HashSet


-- * IO
-------------------------

{-|
Execute an `App`.

Conventionally, this is what should be placed in the @main@ function.
-}
app :: App a -> IO a
app (App io) = io


-- * App
-------------------------

{-|
Encapsulates all errors and crashes.
-}
newtype App a = App (IO a)
  deriving (Functor, Applicative, Monad)

{-|
Execute an effect with all errors handled.
-}
fx :: Fx Void a -> App a
fx (Fx m) = App $ do

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
Can only be executed in `App`.

Calling `fail` causes the executing `App` to interrupt,
killing all of its threads and outputting a message to console.

This function is intended to be used in events which you expect never to happen,
and hence which should be considered bugs.
It is quite similar to calling `fail` on IO,
with a major difference of the error not getting lost in a concurrent environment.
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

sequentially :: Fx err a -> Conc err a
sequentially = Conc
