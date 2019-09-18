module Acquire.Program where

import Acquire.Prelude
import Acquire.Types


instance Semigroup (Program env) where
  (<>) (Program a) (Program b) = Program (a *> b)

instance Monoid (Program env) where
  mempty = Program (pure ())
  mappend = (<>)

instance Contravariant Program where
  contramap envProj (Program impl) = Program (withReaderT envProj impl)

{-|
Lift an accessor, which produces no result or error.
-}
handledAccessor :: (err -> Uio ()) -> Accessor env err () -> Program env
handledAccessor handler (Accessor accessorImpl) =
  Program $ flip mapReaderT accessorImpl $ \ (ExceptT io) -> do
    a <- io
    case a of
      Right () -> return ()
      Left err -> case handler err of
        Uio handlerIo -> handlerIo
