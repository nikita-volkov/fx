module Fx
(
  -- * Eio
  provideAndAccess,
  -- * Provider
  Provider,
  acquireAndRelease,
  -- * Accessor
  Accessor,
  use,
  mapEnv,
  concurrently,
  -- * ConcAccessor
  ConcAccessor,
  sequentially,
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
provideAndAccess :: Provider err env -> Accessor env err res -> Eio err res
provideAndAccess (Provider providerEio) (Accessor accessorRdr) = do
  (env, release) <- providerEio
  Eio.bindErr
    (\ err -> release *> throwError err)
    (runReaderT accessorRdr env <* release)


-- * Provider
-------------------------

{-|
Effectful computation with explicit errors,
which encompasses environment acquisition and releasing.

Composes well, allowing you to merge multiple providers into one.

Builds up on some ideas expressed in http://www.haskellforall.com/2013/06/the-resource-applicative.html
-}
newtype Provider err env = Provider (Eio err (env, Eio err ()))

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

instance EioLifting err (Provider err) where
  liftEio = Provider . fmap (\ a -> (a, return ()))

instance MonadIO (Provider SomeException) where
  liftIO = liftEio @SomeException . liftIO

instance Bifunctor Provider where
  bimap fn1 fn2 (Provider m) = Provider (bimap fn1 (fn2 *** first fn1) m)
  second = fmap

{-|
Create a resource provider from actions that don't fail.
You can turn your exception-throwing actions into these
by means of the `Eio` API.
-}
acquireAndRelease :: Eio err env -> (env -> Eio err ()) -> Provider err env
acquireAndRelease acquire release = Provider (fmap (\ env -> (env, release env)) acquire)


-- * Accessor
-------------------------

{-|
Effectful computation with explicit errors in context of provided environment.
-}
newtype Accessor env err res = Accessor (ReaderT env (Eio err) res)

deriving instance Functor (Accessor env err)
deriving instance Applicative (Accessor env err)
deriving instance Monoid err => Alternative (Accessor env err)
deriving instance Monad (Accessor env err)
deriving instance Monoid err => MonadPlus (Accessor env err)
deriving instance MonadError err (Accessor env err)

instance Bifunctor (Accessor env) where
  first = mapAccessor . mapReaderT . first
  second = fmap

instance EioLifting err (Accessor env err) where
  liftEio = Accessor . lift

instance MonadIO (Accessor env SomeException) where
  liftIO = Accessor . liftIO

mapAccessor :: (ReaderT envA (Eio errA) resA -> ReaderT envB (Eio errB) resB) -> Accessor envA errA resA -> Accessor envB errB resB
mapAccessor mapper (Accessor impl) = Accessor (mapper impl)

{-|
Map the environment of an accessor.
-}
mapEnv :: (b -> a) -> Accessor a err res -> Accessor b err res
mapEnv fn = mapAccessor (withReaderT fn)

{-|
Lift an env-using function into accessor.

This is the way you define accessors.

__Warning__:
This function leaks abstraction.
It is your responsibility to make sure that you don't use the provided @env@
outside of accessor.
-}
use :: (env -> Eio err a) -> Accessor env err a
use = Accessor . ReaderT

{-|
Lift a concurrently composed computation. 
-}
concurrently :: ConcAccessor env err res -> Accessor env err res
concurrently (ConcAccessor accessor) = accessor


-- * ConcAccessor
-------------------------

newtype ConcAccessor env err res = ConcAccessor (Accessor env err res)

deriving instance Functor (ConcAccessor env err)

instance Applicative (ConcAccessor env err) where
  pure = ConcAccessor . pure
  (<*>) (ConcAccessor (Accessor m1)) (ConcAccessor (Accessor m2)) = ConcAccessor $ Accessor $ ReaderT $ \ env ->
    Eio.liftConcEio (liftEio (runReaderT m1 env) <*> liftEio (runReaderT m2 env))

{-|
Lift a sequentially composed computation. 
-}
sequentially :: Accessor env err res -> ConcAccessor env err res
sequentially = ConcAccessor
