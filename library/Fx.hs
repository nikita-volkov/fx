module Fx
(
  -- * Uio
  Uio,
  Uio.exceptionlessIo,
  Uio.handledIo,
  Uio.handledEio,
  Uio.providerAndProcess,
  UioLifting(..),
  -- * Eio
  Eio,
  Eio.providerAndAccessor,
  EioLifting(..),
  -- * Provider
  Provider,
  Provider.acquireAndRelease,
  -- * Accessor
  Accessor,
  Accessor.mapEnv,
  Accessor.mapErr,
  Accessor.mapEnvAndErr,
  -- * Process
  Process,
  Process.handledAccessor,
  -- * Io
  IoLifting,
  io,
)
where

import Fx.Prelude
import Fx.Types
import qualified Fx.Accessor as Accessor
import qualified Fx.Eio as Eio
import qualified Fx.IO as IO
import qualified Fx.Provider as Provider
import qualified Fx.Process as Process
import qualified Fx.Uio as Uio


{-|
Synonym to `liftIO`, conforming to naming conventions of this library.
-}
io :: IoLifting m => IO a -> m a
io = liftIO
