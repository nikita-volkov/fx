module Acquire.Accessor where

import Acquire.Prelude


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
