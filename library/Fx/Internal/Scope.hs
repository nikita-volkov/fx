-- |
-- Scope abstraction for resource management
module Fx.Internal.Scope
  ( -- * Scope
    acquire,
    releasing,
  )
where

import Fx.Prelude
import Fx.Internal.Types
import Fx.Internal.Fx (closeEnv)

-- |
-- Create a resource provider from an acquiring effect.
--
-- To add a release action, use 'releasing'.
acquire :: Fx () err env -> Scope err env
acquire acquireFx = Scope do
  env <- acquireFx
  return (env, pure ())

-- |
-- Add a release action to an existing resource provider.
--
-- Example:
--
-- > fileInWriteMode :: FilePath -> Scope IOError Handle
-- > fileInWriteMode path =
-- >   releasing hClose (acquire (openFile path WriteMode))
releasing :: Fx env err () -> Scope err env -> Scope err env
releasing release (Scope m) = Scope $ do
  (env, existingRelease) <- m
  return (env, existingRelease >> closeEnv env release)
