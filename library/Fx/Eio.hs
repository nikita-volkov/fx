module Fx.Eio where

import Fx.Prelude
import Fx.Types


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

instance MonadIO (Eio SomeException) where
  liftIO = io

instance UioLifting (Eio err) where
  uio (Uio io) = exceptionlessIo io

instance EioLifting err (Eio err) where
  eio = id

mapImp :: (ExceptT err1 IO res1 -> ExceptT err2 IO res2) -> Eio err1 res1 -> Eio err2 res2
mapImp fn (Eio imp) = Eio (fn imp)

{-|
Turn an exception-throwing IO into an action with explicit errors.
-}
io :: IO res -> Eio SomeException res
io = Eio . ExceptT . try

exceptionlessIo :: IO res -> Eio err res
exceptionlessIo = Eio . lift

{-|
Retry an action the specified amount of times.
-}
retry :: Word -> Eio err res -> Eio err res
retry times = mapImp $ mapExceptT $ \ io -> let
  loop n = \ case
    Right res -> return (Right res)
    Left err -> if n > 0
      then io >>= loop (pred n)
      else return (Left err)
  in io >>= loop times

{-|
Handle error in another failing action.
-}
bindErr :: (a -> Eio b res) -> Eio a res -> Eio b res
bindErr handler = mapImp $ mapExceptT $ \ io -> do
  a <- io
  case a of
    Right res -> return (Right res)
    Left err -> case handler err of
      Eio (ExceptT handlerIo) -> handlerIo

orElse :: Eio err res -> Eio err res -> Eio err res
orElse (Eio (ExceptT io2)) (Eio (ExceptT io1)) = Eio $ ExceptT $ do
  a <- io1
  case a of
    Right res -> return (Right res)
    Left err -> io2

{-|
Having an environment provider, execute an action,
which uses the environment and produces either an error or result.
-}
providerAndAccessor :: Provider env -> Accessor env err res -> Eio err res
providerAndAccessor (Provider providerIo) (Accessor accessorRdr) =
  Eio (ExceptT (bracket providerIo snd (runExceptT . runReaderT accessorRdr . fst)))
