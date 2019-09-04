module Acquire
where

import Acquire.Prelude


-- * IO
-------------------------

{-|
Execute an action, which uses a resource,
having a resource provider.
-}
acquireAndUse :: Acquire env -> Use env err res -> IO (Either err res)
acquireAndUse (Acquire acquireIo) (Use useRdr) =
  bracket acquireIo snd (runExceptT . runReaderT useRdr . fst)


-- * Acquire
-------------------------

{-|
Resource provider.
Abstracts over resource acquisition and releasing.

Composes well, allowing you to merge multiple providers into one.

Implementation of http://www.haskellforall.com/2013/06/the-resource-applicative.html
-}
newtype Acquire env =
  Acquire (IO (env, IO ()))

instance Functor Acquire where
  fmap f (Acquire io) =
    Acquire $ do
      (env, release) <- io
      return (f env, release)

instance Applicative Acquire where
  pure env =
    Acquire (pure (env, pure ()))
  Acquire io1 <*> Acquire io2 =
    Acquire $ do
      (f, release1) <- io1
      (x, release2) <- onException io2 release1
      return (f x, release2 >> release1)

instance Monad Acquire where
  return = pure
  (>>=) (Acquire io1) k2 =
    Acquire $ do
      (resource1, release1) <- io1
      (resource2, release2) <- case k2 resource1 of Acquire io2 -> onException io2 release1
      return (resource2, release2 >> release1)

instance MonadIO Acquire where
  liftIO io =
    Acquire (fmap (, return ()) io)


-- * Use
-------------------------

{-|
Resource handler, which has a notion of pure errors.
-}
newtype Use env err res = Use (ReaderT env (ExceptT err IO) res)
  deriving (Functor, Applicative, Alternative, Monad, MonadPlus)

instance Bifunctor (Use env) where
  first = mapErr
  second = fmap

{-|
Map the environment of a resource handler.
-}
mapEnv :: (b -> a) -> Use a err res -> Use b err res
mapEnv fn (Use rdr) = Use (withReaderT fn rdr)

{-|
Map the error of a resource handler.
-}
mapErr :: (a -> b) -> Use env a res -> Use env b res
mapErr fn (Use rdr) = Use (mapReaderT (withExceptT fn) rdr)

{-|
Map both the environment and the error of a resource handler.
-}
mapEnvAndErr :: (envB -> envA) -> (errA -> errB) -> Use envA errA res -> Use envB errB res
mapEnvAndErr envProj errProj (Use rdr) = Use (withReaderT envProj (mapReaderT (withExceptT errProj) rdr))
