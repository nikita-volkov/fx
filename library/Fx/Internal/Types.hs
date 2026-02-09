-- |
-- Internal shared types and core definitions
module Fx.Internal.Types
  ( -- * Core Types
    Fx (..),
    FxEnv (..),
    Scope (..),
    Future (..),
    Conc (..),

    -- * Exception Types
    FxException (..),
    FxExceptionReason (..),

    -- * Utilities
    mapFx,
    mapFuture,
    mapCompose,
  )
where

import Fx.Prelude
import qualified Fx.Strings as Strings

-- * Core Types

-------------------------

-- |
-- Effectful computation with explicit errors in the context of provided environment.
--
-- Think of it as a combination of `ReaderT` and `ExceptT` over `IO`, with some extra features for concurrency and error handling.
newtype Fx env err res = Fx (ReaderT (FxEnv env) (ExceptT err IO) res)

deriving instance Functor (Fx env err)

deriving instance Applicative (Fx env err)

deriving instance (Monoid err) => Alternative (Fx env err)

deriving instance Monad (Fx env err)

deriving instance (Monoid err) => MonadPlus (Fx env err)

instance MonadFail (Fx env err) where
  fail msg = Fx $
    ReaderT $
      \(FxEnv _ crash _) -> liftIO $ do
        crash [] (ErrorCallFxExceptionReason (ErrorCall msg))
        fail "Crashed"

instance MonadIO (Fx env SomeException) where
  liftIO io = Fx (ReaderT (\(FxEnv unmask _ _) -> ExceptT (try (unmask io))))

instance MonadError err (Fx env err) where
  throwError err = Fx (lift (throwE err))
  catchError fx handler = Fx $ ReaderT $ \fxEnv -> ExceptT $ do
    a <- runExceptT (let Fx m = fx in runReaderT m fxEnv)
    case a of
      Right res -> return (Right res)
      Left err -> case handler err of
        Fx m -> runExceptT (runReaderT m fxEnv)

instance Bifunctor (Fx env) where
  bimap lf rf = mapFx (mapReaderT (mapExceptT (fmap (bimap lf rf))))

mapFx ::
  ( ReaderT (FxEnv env1) (ExceptT err1 IO) res1 ->
    ReaderT (FxEnv env2) (ExceptT err2 IO) res2
  ) ->
  Fx env1 err1 res1 ->
  Fx env2 err2 res2
mapFx fn (Fx m) = Fx (fn m)

-- |
-- Runtime and application environment.
data FxEnv env
  = FxEnv
      -- | Unmasking function.
      (forall a. IO a -> IO a)
      -- | Crash.
      ([ThreadId] -> FxExceptionReason -> IO ())
      -- | User environment.
      env

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
-- Handle to a result of an action which may still be being executed on another thread.
newtype Future err res = Future (Compose STM (Either (Maybe err)) res)
  deriving (Functor, Applicative, Alternative)

instance Bifunctor Future where
  bimap lf rf = mapFuture (mapCompose (fmap (bimap (fmap lf) rf)))

mapFuture ::
  ( Compose STM (Either (Maybe err1)) res1 ->
    Compose STM (Either (Maybe err2)) res2
  ) ->
  Future err1 res1 ->
  Future err2 res2
mapFuture fn (Future m) = Future (fn m)

-- |
-- Wrapper over `Fx`, whose instances compose by running computations on separate threads.
newtype Conc env err res = Conc (Fx env err res)

deriving instance Functor (Conc env err)

deriving instance Bifunctor (Conc env)

-- * Exceptions

-------------------------

-- |
-- Fatal failure of an `Fx` application.
data FxException = FxException [ThreadId] FxExceptionReason

instance Show FxException where
  show (FxException tids reason) = Strings.fatalErrorAtThreadPath tids (show reason)

instance Exception FxException

-- |
-- Reason of a fatal failure of an `Fx` application.
data FxExceptionReason
  = UncaughtExceptionFxExceptionReason SomeException
  | ErrorCallFxExceptionReason ErrorCall
  | BugFxExceptionReason String

instance Show FxExceptionReason where
  show = \case
    UncaughtExceptionFxExceptionReason exc -> Strings.uncaughtException exc
    ErrorCallFxExceptionReason errorCall -> show errorCall
    BugFxExceptionReason details -> Strings.bug details
