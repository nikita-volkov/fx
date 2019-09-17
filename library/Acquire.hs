module Acquire
where

import Acquire.Prelude


-- * IO
-------------------------

{-|
Having an environment provider, execute an action,
which uses the environment and produces either an error or result.
-}
providerAndAccessor :: Provider env -> Accessor env err res -> IO (Either err res)
providerAndAccessor (Provider providerIo) (Accessor accessorRdr) =
  bracket providerIo snd (runExceptT . runReaderT accessorRdr . fst)

{-|
Having an environment provider, execute an action,
which uses the environment and encapsulates result and error handling,
-}
providerAndProgram :: Provider env -> Program env -> IO ()
providerAndProgram (Provider providerIo) (Program programRdr) =
  bracket providerIo snd (runReaderT programRdr . fst)


-- * Provider
-------------------------

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


-- * Accessor
-------------------------

{-|
Environment handler, which has a notion of pure errors.
-}
newtype Accessor env err res = Accessor (ReaderT env (ExceptT err IO) res)
  deriving (Functor, Applicative, Alternative, Monad, MonadPlus, MonadIO, MonadError err)

instance Bifunctor (Accessor env) where
  first = mapErr
  second = fmap

mapImpl :: (ReaderT envA (ExceptT errA IO) resA -> ReaderT envB (ExceptT errB IO) resB) -> Accessor envA errA resA -> Accessor envB errB resB
mapImpl mapper (Accessor impl) = Accessor (mapper impl)

{-|
Map the environment of an accessor.
-}
mapEnv :: (b -> a) -> Accessor a err res -> Accessor b err res
mapEnv fn = mapImpl (withReaderT fn)

{-|
Map the error of an accessor.
-}
mapErr :: (a -> b) -> Accessor env a res -> Accessor env b res
mapErr fn = mapImpl (mapReaderT (withExceptT fn))

{-|
Map both the environment and the error of an accessor.
-}
mapEnvAndErr :: (envB -> envA) -> (errA -> errB) -> Accessor envA errA res -> Accessor envB errB res
mapEnvAndErr envProj errProj = mapImpl (withReaderT envProj . mapReaderT (withExceptT errProj))

{-|
Expose the error in result,
producing an accessor, which is compatible with any error type.

This function is particularly helpful, when you need to map into error of type `Void`.
-}
exposeErr :: Accessor env err res -> Accessor env anyErr (Either err res)
exposeErr = mapImpl $ mapReaderT $ mapExceptT $ fmap $ Right

{-|
Map from error to result, leaving the error be anything.

This function is particularly helpful, when you need to map into error of type `Void`.
-}
absorbErr :: (err -> res) -> Accessor env err res -> Accessor env anyErr res
absorbErr errProj = mapImpl $ mapReaderT $ mapExceptT $ fmap $ either (Right . errProj) Right

{-|
Map error monadically.
-}
bindErr :: (a -> Accessor env b res) -> Accessor env a res -> Accessor env b res
bindErr lifter (Accessor aImpl) = Accessor $ ReaderT $ \ env -> ExceptT $ do
  resEither <- runExceptT (runReaderT aImpl env)
  case resEither of
    Left a -> case lifter a of
      Accessor bImpl -> runExceptT (runReaderT bImpl env)
    Right res -> return (Right res)

{-|
Lift a terminating action into an accessor, which produces no result and
is compatible with any error type.
-}
program :: Program env -> Accessor env err ()
program (Program impl) = Accessor $ mapReaderT lift impl


-- * Program
-------------------------

{-|
Accessor on an environment with errors and result handlers fully encapsulated.
IOW, it is forced to handle errors internally.
-}
newtype Program env = Program (ReaderT env IO ())

instance Semigroup (Program env) where
  (<>) (Program a) (Program b) = Program (a *> b)

instance Monoid (Program env) where
  mempty = Program (pure ())
  mappend = (<>)

instance Contravariant Program where
  contramap envProj (Program impl) = Program (withReaderT envProj impl)

{-|
Lift an accessor, which produces no result or error.

Functions like `exposeErr`, `absorbErr` and `bindErr`
will help you map to the `Void` error type.
-}
accessor :: Accessor env Void () -> Program env
accessor (Accessor accessorImpl) = Program $ mapReaderT (fmap (const ()) . runExceptT) accessorImpl
