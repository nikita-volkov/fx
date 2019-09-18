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

Functions like `exposeErr`, `absorbErr` and `bindErr`
will help you map to the `Void` error type.
-}
accessor :: Accessor env Void () -> Program env
accessor (Accessor accessorImpl) = Program $ mapReaderT (fmap (const ()) . runExceptT) accessorImpl
