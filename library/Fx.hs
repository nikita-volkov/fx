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
provideAndAccess :: Provider env -> Accessor env err res -> Eio err res
provideAndAccess (Provider providerEio) (Accessor accessorRdr) = do
  (env, release) <- first absurd providerEio
  Eio.bindErr
    (\ err -> first absurd release *> throwError err)
    (runReaderT accessorRdr env <* first absurd release)


-- * Provider
-------------------------

{-|
Environment provider.
Encompasses resource acquisition, releasing and handling of all related errors.

Composes well, allowing you to merge multiple providers into one.

Builds up on some ideas expressed in http://www.haskellforall.com/2013/06/the-resource-applicative.html
-}
newtype Provider env = Provider (Eio Void (env, Eio Void ()))

instance Functor Provider where
  fmap f (Provider m) = Provider $ do
    (env, release) <- m
    return (f env, release)

instance Applicative Provider where
  pure env = Provider (pure (env, pure ()))
  Provider m1 <*> Provider m2 = Provider $
    liftA2 (\ (env1, release1) (env2, release2) -> (env1 env2, release2 *> release1)) m1 m2

instance Monad Provider where
  return = pure
  (>>=) (Provider m1) k2 = Provider $ do
    (env1, release1) <- m1
    (env2, release2) <- case k2 env1 of Provider m2 -> m2
    return (env2, release2 >> release1)

instance EioLifting Void Provider where
  liftEio = Provider . fmap (\ a -> (a, return ()))

{-|
Create a resource provider from actions that don't fail.
You can turn your exception-throwing actions into these
by means of the `Eio` API.
-}
acquireAndRelease :: Eio Void env -> (env -> Eio Void ()) -> Provider env
acquireAndRelease acquire release = Provider (fmap (\ env -> (env, release env)) acquire)


-- * Accessor
-------------------------

{-|
Environment handler, which has a notion of pure errors.
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
-}
use :: (env -> Eio err ()) -> Accessor env err ()
use = Accessor . ReaderT
