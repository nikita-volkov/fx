module Acquire.Eio where

import Acquire.Prelude


{-|
IO with explicit exceptions.
-}
newtype Eio err res = Eio (ExceptT err IO res)

deriving instance Functor (Eio err)
deriving instance Applicative (Eio err)
deriving instance Monoid err => Alternative (Eio err)
deriving instance Monad (Eio err)
deriving instance MonadFix (Eio err)
deriving instance Monoid err => MonadPlus (Eio err)
deriving instance Apply (Eio err)
deriving instance Bind (Eio err)
deriving instance Semigroup err => Alt (Eio err)
deriving instance Monoid err => Plus (Eio err)
instance Bifunctor Eio where
  bimap lf rf = mapImp (mapExceptT (fmap (bimap lf rf)))
instance MonadFail (Eio String) where
  fail = Eio . throwE

mapImp :: (ExceptT err1 IO res1 -> ExceptT err2 IO res2) -> Eio err1 res1 -> Eio err2 res2
mapImp fn (Eio imp) = Eio (fn imp)

io :: IO res -> Eio SomeException res
io = Eio . ExceptT . try

exceptionlessIo :: IO res -> Eio err res
exceptionlessIo = Eio . lift
