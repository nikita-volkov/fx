module Fx.Uio where

import Fx.Prelude
import Fx.Types


deriving instance Functor Uio
deriving instance Applicative Uio
deriving instance Monad Uio
deriving instance MonadFix Uio
deriving instance Apply Uio
deriving instance Bind Uio
deriving instance Semigroup res => Semigroup (Uio res)
deriving instance Monoid res => Monoid (Uio res)

instance UioLifting Uio where
  uio = id

instance EioLifting Void Uio where
  eio = handledEio absurd

mapImp :: (IO res1 -> IO res2) -> Uio res1 -> Uio res2
mapImp fn (Uio imp) = Uio (fn imp)

{-|
Turn IO action into a non-failing action.
It is your responsibility to ensure that it does not throw exceptions.

For this reason an instance of MonadIO is not provided.
-}
exceptionlessIo :: IO res -> Uio res
exceptionlessIo = Uio

{-|
Turn a failing action into a non-failing one,
by providing an exception-handler.
-}
handledIo :: (SomeException -> Uio res) -> IO res -> Uio res
handledIo handler = Uio . handle ((\ (Uio io) -> io) . handler)

{-|
Turn a failing action into a non-failing one,
by providing an error-handler.
-}
handledEio :: (err -> Uio res) -> Eio err res -> Uio res
handledEio handler (Eio (ExceptT io)) = Uio $ do
  a <- io
  case a of
    Right res -> return res
    Left err -> case handler err of
      Uio handlerIo -> handlerIo

{-|
Having an environment provider, execute an action,
which uses the environment and encapsulates result and error handling,
-}
providerAndProcess :: Provider env -> Process env -> Uio ()
providerAndProcess (Provider providerIo) (Process processRdr) =
  Uio (bracket providerIo snd (runReaderT processRdr . fst))
