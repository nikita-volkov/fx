module Acquire
where

import Acquire.Prelude


-- * IO
-------------------------

{-|
Having a resource provider, execute an action,
which uses the resource and produces either an error or result.
-}
acquireAndUse :: Acquire env -> Use env err res -> IO (Either err res)
acquireAndUse (Acquire acquireIo) (Use useRdr) =
  bracket acquireIo snd (runExceptT . runReaderT useRdr . fst)

{-|
Having a resource provider, execute an action,
which uses the resource and encapsulates result and error handling,
-}
acquireAndTerminate :: Acquire env -> Terminate env -> IO ()
acquireAndTerminate (Acquire acquireIo) (Terminate terminateRdr) =
  bracket acquireIo snd (runReaderT terminateRdr . fst)


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
      (env1, release1) <- io1
      (env2, release2) <- case k2 env1 of Acquire io2 -> onException io2 release1
      return (env2, release2 >> release1)

instance MonadIO Acquire where
  liftIO io =
    Acquire (fmap (, return ()) io)


-- * Use
-------------------------

{-|
Resource handler, which has a notion of pure errors.
-}
newtype Use env err res = Use (ReaderT env (ExceptT err IO) res)
  deriving (Functor, Applicative, Alternative, Monad, MonadPlus, MonadIO)

instance Bifunctor (Use env) where
  first = mapErr
  second = fmap

mapImpl :: (ReaderT envA (ExceptT errA IO) resA -> ReaderT envB (ExceptT errB IO) resB) -> Use envA errA resA -> Use envB errB resB
mapImpl mapper (Use impl) = Use (mapper impl)

{-|
Map the environment of a resource handler.
-}
mapEnv :: (b -> a) -> Use a err res -> Use b err res
mapEnv fn = mapImpl (withReaderT fn)

{-|
Map the error of a resource handler.
-}
mapErr :: (a -> b) -> Use env a res -> Use env b res
mapErr fn = mapImpl (mapReaderT (withExceptT fn))

{-|
Map both the environment and the error of a resource handler.
-}
mapEnvAndErr :: (envB -> envA) -> (errA -> errB) -> Use envA errA res -> Use envB errB res
mapEnvAndErr envProj errProj = mapImpl (withReaderT envProj . mapReaderT (withExceptT errProj))

{-|
Map from error to result, leaving the error be anything.

This function is particularly helpful, when you need to map into error of type `Void`.
-}
absorbErr :: (err -> res) -> Use env err res -> Use env anyErr res
absorbErr errProj = mapImpl $ mapReaderT $ mapExceptT $ fmap $ either (Right . errProj) Right

{-|
Map error monadically.
-}
bindErr :: (a -> Use env b res) -> Use env a res -> Use env b res
bindErr lifter (Use aImpl) = Use $ ReaderT $ \ env -> ExceptT $ do
  resEither <- runExceptT (runReaderT aImpl env)
  case resEither of
    Left a -> case lifter a of
      Use bImpl -> runExceptT (runReaderT bImpl env)
    Right res -> return (Right res)


-- * Terminate
-------------------------

{-|
Fully encapsulated action on an environment producing no results or errors.
-}
newtype Terminate env = Terminate (ReaderT env IO ())

instance Semigroup (Terminate env) where
  (<>) (Terminate a) (Terminate b) = Terminate (a *> b)

instance Monoid (Terminate env) where
  mempty = Terminate (pure ())
  mappend = (<>)

instance Contravariant Terminate where
  contramap envProj (Terminate impl) = Terminate (withReaderT envProj impl)

{-|
Lift a use, which produces no result or error.

Functions like `absorbErr` and `bindErr`
will help you map to the `Void` error type.
-}
use :: Use env Void () -> Terminate env
use (Use useImpl) = Terminate $ mapReaderT (fmap (const ()) . runExceptT) useImpl
