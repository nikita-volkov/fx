module Fx.Accessor where

import Fx.Prelude
import Fx.Types


deriving instance Functor (Accessor env err)
deriving instance Applicative (Accessor env err)
deriving instance Monoid err => Alternative (Accessor env err)
deriving instance Monad (Accessor env err)
deriving instance Monoid err => MonadPlus (Accessor env err)
deriving instance MonadError err (Accessor env err)

instance Bifunctor (Accessor env) where
  first = mapErr
  second = fmap

instance UioLifting (Accessor env err) where
  uio (Uio io) = Accessor (liftIO io)

instance EioLifting err (Accessor env err) where
  eio (Eio impl) = Accessor (lift impl)

instance MonadIO (Accessor env SomeException) where
  liftIO io = Accessor (lift (ExceptT (try io)))

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
process :: Process env -> Accessor env err ()
process (Process impl) = Accessor $ mapReaderT lift impl
