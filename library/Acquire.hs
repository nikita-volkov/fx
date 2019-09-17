module Acquire
(
  -- * IO
  providerAndAccessor,
  providerAndProgram,
  -- * Provider
  Provider(..),
  -- * Accessor
  Accessor,
  Accessor.mapEnv,
  Accessor.mapErr,
  Accessor.mapEnvAndErr,
  Accessor.exposeErr,
  Accessor.absorbErr,
  Accessor.bindErr,
  program,
  -- * Program
  Program,
  accessor,
)
where

import Acquire.Prelude
import Acquire.Accessor (Accessor(..))
import Acquire.Provider (Provider(..))
import Acquire.Program (Program(..))
import qualified Acquire.Accessor as Accessor
import qualified Acquire.Provider as Provider
import qualified Acquire.Program as Program


-- * IO
-------------------------

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


-- * Accessor
-------------------------

{-|
Lift a terminating action into an accessor, which produces no result and
is compatible with any error type.
-}
program :: Program env -> Accessor env err ()
program (Program impl) = Accessor $ mapReaderT lift impl


-- * Program
-------------------------

{-|
Lift an accessor, which produces no result or error.

Functions like `exposeErr`, `absorbErr` and `bindErr`
will help you map to the `Void` error type.
-}
accessor :: Accessor env Void () -> Program env
accessor (Accessor accessorImpl) = Program $ mapReaderT (fmap (const ()) . runExceptT) accessorImpl
