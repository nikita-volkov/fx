module Acquire.Eio where

import Acquire.Prelude
import Acquire.Types


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

{-|
Turn an exception-throwing IO into an action with explicit errors.
-}
io :: IO res -> Eio SomeException res
io = Eio . ExceptT . try

exceptionlessIo :: IO res -> Eio err res
exceptionlessIo = Eio . lift

retry :: Word -> Eio [err] res -> Eio [err] res
retry times eio = asum1 (eio :| replicate (pred (fromIntegral times)) eio)

bindErr :: (a -> Eio b res) -> Eio a res -> Eio b res
bindErr = error "TODO"

{-|
Having an environment provider, execute an action,
which uses the environment and produces either an error or result.
-}
providerAndAccessor :: Provider env -> Accessor env err res -> Eio err res
providerAndAccessor (Provider providerIo) (Accessor accessorRdr) =
  Eio (ExceptT (bracket providerIo snd (runExceptT . runReaderT accessorRdr . fst)))
