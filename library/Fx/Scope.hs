-- |
-- Scope abstraction for resource management
module Fx.Scope
  ( -- * Scope
    Scope (..),
    acquire,
    releasing,
    scoping,
  )
where

import Fx.Fx (Fx (..), FxEnv (..), RunsFx (..))
import Fx.Prelude

-- |
-- Instructions of how to acquire and release a resource of type `env` and which may fail with `err`.
newtype Scope err env = Scope (Fx () err (env, Fx () err ()))

instance Functor (Scope err) where
  fmap f (Scope m) = Scope $ do
    (env, release) <- m
    return (f env, release)

instance Applicative (Scope err) where
  pure env = Scope (pure (env, pure ()))
  Scope m1 <*> Scope m2 =
    Scope $
      liftA2 (\(env1, release1) (env2, release2) -> (env1 env2, release2 *> release1)) m1 m2

instance Bifunctor Scope where
  bimap lf rf (Scope m) = Scope (bimap lf (bimap rf (first lf)) m)
  second = fmap

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

-- |
-- Execute Fx in the scope of a provided environment.
scoping :: Scope err env -> Fx env err res -> Fx env' err res
scoping (Scope (Fx acquire)) (Fx fx) =
  Fx $
    ReaderT $
      \(FxEnv unmask crash _) -> ExceptT $ do
        let providerFxEnv = FxEnv unmask crash ()
        acquisition <- runExceptT (runReaderT acquire providerFxEnv)
        case acquisition of
          Left err -> return (Left err)
          Right (env, (Fx release)) -> do
            resOrErr <- runExceptT (runReaderT fx (FxEnv unmask crash env))
            releasing <- runExceptT (runReaderT release providerFxEnv)
            return (resOrErr <* releasing)

closeEnv :: env -> Fx env err res -> Fx env' err res
closeEnv env (Fx fx) =
  Fx $
    ReaderT $
      \(FxEnv unmask crash _) ->
        ExceptT $
          runExceptT (runReaderT fx (FxEnv unmask crash env))

instance RunsFx () err (Scope err) where
  runFx fx = Scope (fmap (\env -> (env, pure ())) fx)
