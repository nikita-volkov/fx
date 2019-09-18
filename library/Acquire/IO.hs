module Acquire.IO
where

import Acquire.Prelude
import Acquire.Types


instance UioLifting IO where
  uio (Uio io) = io

instance EioLifting SomeException IO where
  eio (Eio (ExceptT io)) = io >>= either throwIO return

{-|
Having an environment provider, execute an action,
which uses the environment and produces either an error or result.
-}
providerAndAccessor :: Provider env -> Accessor env err res -> IO (Either err res)
providerAndAccessor (Provider providerIo) (Accessor accessorRdr) =
  bracket providerIo snd (runExceptT . runReaderT accessorRdr . fst)

{-|
Having an environment provider, execute an action,
which uses the environment and encapsulates result and error handling,
-}
providerAndProgram :: Provider env -> Program env -> IO ()
providerAndProgram (Provider providerIo) (Program programRdr) =
  bracket providerIo snd (runReaderT programRdr . fst)
