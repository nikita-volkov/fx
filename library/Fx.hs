module Fx
(
  -- * Eio
  provideAndAccess,
  -- * Provider
  Pdr,
  acquireAndRelease,
  -- * Accessor
  Asr,
  use,
  mapEnv,
  fork,
)
where

import Fx.Prelude
import qualified Exceptionless as Eio


-- * Eio
-------------------------

{-|
Having an environment provider, execute an action,
which uses the environment and produces either an error or result.
-}
provideAndAccess :: Pdr err env -> Asr env err res -> Eio err res
provideAndAccess (Pdr providerEio) (Asr accessorRdr) = do
  (env, release) <- providerEio
  Eio.bindErr
    (\ err -> release *> throwError err)
    (runReaderT accessorRdr env <* release)


-- * Provider
-------------------------

{-|
Provider.
Effectful computation with explicit errors,
which encompasses environment acquisition and releasing.

Composes well, allowing you to merge multiple providers into one.

Builds up on some ideas expressed in http://www.haskellforall.com/2013/06/the-resource-applicative.html
-}
newtype Pdr err env = Pdr (Eio err (env, Eio err ()))

instance Functor (Pdr err) where
  fmap f (Pdr m) = Pdr $ do
    (env, release) <- m
    return (f env, release)

instance Applicative (Pdr err) where
  pure env = Pdr (pure (env, pure ()))
  Pdr m1 <*> Pdr m2 = Pdr $
    liftA2 (\ (env1, release1) (env2, release2) -> (env1 env2, release2 *> release1)) m1 m2

instance Monad (Pdr err) where
  return = pure
  (>>=) (Pdr m1) k2 = Pdr $ do
    (env1, release1) <- m1
    (env2, release2) <- case k2 env1 of Pdr m2 -> m2
    return (env2, release2 >> release1)

instance EioLifting err (Pdr err) where
  liftEio = Pdr . fmap (\ a -> (a, return ()))

instance MonadIO (Pdr SomeException) where
  liftIO = liftEio @SomeException . liftIO

instance Bifunctor Pdr where
  bimap fn1 fn2 (Pdr m) = Pdr (bimap fn1 (fn2 *** first fn1) m)
  second = fmap

{-|
Create a resource provider from actions that don't fail.
You can turn your exception-throwing actions into these
by means of the `Eio` API.
-}
acquireAndRelease :: Eio err env -> (env -> Eio err ()) -> Pdr err env
acquireAndRelease acquire release = Pdr (fmap (\ env -> (env, release env)) acquire)


-- * Accessor
-------------------------

{-|
Accessor.
Effectful computation with explicit errors in context of provided environment.
-}
newtype Asr env err res = Asr (ReaderT env (Eio err) res)

deriving instance Functor (Asr env err)
deriving instance Applicative (Asr env err)
deriving instance Monoid err => Alternative (Asr env err)
deriving instance Monad (Asr env err)
deriving instance Monoid err => MonadPlus (Asr env err)
deriving instance MonadError err (Asr env err)

instance Bifunctor (Asr env) where
  first = mapAsr . mapReaderT . first
  second = fmap

instance EioLifting err (Asr env err) where
  liftEio = Asr . lift

instance MonadIO (Asr env SomeException) where
  liftIO = Asr . liftIO

mapAsr :: (ReaderT envA (Eio errA) resA -> ReaderT envB (Eio errB) resB) -> Asr envA errA resA -> Asr envB errB resB
mapAsr mapper (Asr impl) = Asr (mapper impl)

{-|
Map the environment of an accessor.
-}
mapEnv :: (b -> a) -> Asr a err res -> Asr b err res
mapEnv fn = mapAsr (withReaderT fn)

{-|
Lift an env-using function into accessor.

This is the way you define accessors.

__Warning__:
This function leaks abstraction.
It is your responsibility to make sure that you don't use the provided @env@
outside of accessor.
-}
use :: (env -> Eio err a) -> Asr env err a
use = Asr . ReaderT

{-|
Fork a computation to be run on a separate thread,
blocking until it finishes or fails,
while also performing a computation on current thread.
-}
fork :: Asr env err () -> Asr env err res -> Asr env err res
fork (Asr fork) (Asr main) = Asr $ ReaderT $ \ env -> do
  blockVar <- Eio.liftSafeIO newEmptyMVar
  Eio.fork $ let
    handler err = Eio.liftSafeIO (putMVar blockVar (Just err))
    in Eio.bindErr handler (runReaderT fork env)
  mainRes <- runReaderT main env
  blockRes <- Eio.liftSafeIO (takeMVar blockVar)
  case blockRes of
    Just err -> throwError err
    Nothing -> return ()
  return mainRes
