module Acquire.Provider where

import Acquire.Prelude


{-|
Environment provider.
Abstracts over resource acquisition and releasing.

Composes well, allowing you to merge multiple providers into one.

Implementation of http://www.haskellforall.com/2013/06/the-resource-applicative.html
-}
newtype Provider env =
  Provider (IO (env, IO ()))

instance Functor Provider where
  fmap f (Provider io) =
    Provider $ do
      (env, release) <- io
      return (f env, release)

instance Applicative Provider where
  pure env =
    Provider (pure (env, pure ()))
  Provider io1 <*> Provider io2 =
    Provider $ do
      (f, release1) <- io1
      (x, release2) <- onException io2 release1
      return (f x, release2 >> release1)

instance Monad Provider where
  return = pure
  (>>=) (Provider io1) k2 =
    Provider $ do
      (env1, release1) <- io1
      (env2, release2) <- case k2 env1 of Provider io2 -> onException io2 release1
      return (env2, release2 >> release1)

instance MonadIO Provider where
  liftIO io =
    Provider (fmap (, return ()) io)
