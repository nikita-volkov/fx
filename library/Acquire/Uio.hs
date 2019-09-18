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

io :: (SomeException -> Uio res) -> IO res -> Uio res
io handler = Uio . handle ((\ (Uio io) -> io) . handler)

exceptionlessIo :: IO res -> Uio res
exceptionlessIo = Uio

eio :: (err -> Uio res) -> Eio err res -> Uio res
eio handler (Eio (ExceptT io)) = Uio $ do
  a <- io
  case a of
    Right res -> return res
    Left err -> case handler err of
      Uio handlerIo -> handlerIo
