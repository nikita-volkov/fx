module Acquire.Uio where

import Acquire.Prelude
import Acquire.Eio (Eio(..))
import qualified Acquire.Eio as Eio


{-|
Unexceptional IO,
i.e., IO which only throws exceptions in case of bugs.
-}
newtype Uio res = Uio (IO res)

deriving instance Functor Uio
deriving instance Applicative Uio
deriving instance Monad Uio
deriving instance MonadFix Uio
deriving instance Apply Uio
deriving instance Bind Uio
deriving instance Semigroup res => Semigroup (Uio res)
deriving instance Monoid res => Monoid (Uio res)

mapImp :: (IO res1 -> IO res2) -> Uio res1 -> Uio res2
mapImp fn (Uio imp) = Uio (fn imp)

io :: IO res -> Uio (Either SomeException res)
io = Uio . try

exceptionlessIo :: IO res -> Uio res
exceptionlessIo = Uio

eio :: Eio err res -> Uio (Either err res)
eio (Eio (ExceptT io)) = Uio io
