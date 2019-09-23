module Fx.Types where

import Fx.Prelude


{-|
Unexceptional IO,
i.e., IO which only throws exceptions in case of bugs.
-}
newtype Uio res = Uio (IO res)

{-|
IO with explicit exceptions.
-}
newtype Eio err res = Eio (ExceptT err IO res)

{-|
Environment provider.
Encompasses resource acquisition, releasing and handling of all related errors.

Composes well, allowing you to merge multiple providers into one.

Builds up on some ideas expressed in http://www.haskellforall.com/2013/06/the-resource-applicative.html
-}
newtype Provider env = Provider (IO (env, IO ()))

{-|
Accessor on an environment with errors and result handlers fully encapsulated.
IOW, it is forced to handle errors internally.
-}
newtype Process env = Process (ReaderT env IO ())

{-|
Environment handler, which has a notion of pure errors.
-}
newtype Accessor env err res = Accessor (ReaderT env (ExceptT err IO) res)

{-|
Support for lifting of unexceptional IO.
-}
class UioLifting m where
  uio :: Uio a -> m a

{-|
Support for lifting of IO with explicit exceptions.
-}
class EioLifting err m | m -> err where
  eio :: Eio err a -> m a
