module Acquire.Program where

import Acquire.Prelude


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
