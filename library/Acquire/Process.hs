module Acquire.Process where

import Acquire.Prelude
import Acquire.Types


instance Semigroup (Process env) where
  (<>) (Process a) (Process b) = Process (a *> b)

instance Monoid (Process env) where
  mempty = Process (pure ())
  mappend = (<>)

instance Contravariant Process where
  contramap envProj (Process impl) = Process (withReaderT envProj impl)

{-|
Lift an accessor, which produces no result or error.
-}
handledAccessor :: (err -> Uio ()) -> Accessor env err () -> Process env
handledAccessor handler (Accessor accessorImpl) =
  Process $ flip mapReaderT accessorImpl $ \ (ExceptT io) -> do
    a <- io
    case a of
      Right () -> return ()
      Left err -> case handler err of
        Uio handlerIo -> handlerIo
