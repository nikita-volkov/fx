{-# OPTIONS_GHC -Wno-orphans #-}

module Fx
  ( -- * Fx
    Fx,

    -- ** Execution
    RunsFx (..),

    -- ** Environment handling
    scoping,
    mapEnv,

    -- ** Error handling
    throwErr,
    handleErr,
    mapErr,
    exposeErr,
    absorbErr,

    -- ** Concurrency
    concurrently,

    -- ** IO execution

    -- |
    -- These functions leak abstraction in one way or the other,
    -- requiring you to ensure that your code doesn't throw unexpected exceptions.
    -- `try` and `catch` are your tools for that.
    --
    -- Besides these functions `Fx` also has an instance of `MonadIO`,
    -- which provides the only non-leaky way of running IO, catching all possible exceptions.
    runTotalIO,
    runPartialIO,
    runExceptionalIO,

    -- * Scope
    Scope,
    acquire,
    registerRelease,

    -- * Exceptions
    FxException (..),
    FxExceptionReason (..),
  )
where

import Fx.Conc
import Fx.Fx
import Fx.Prelude
import Fx.Scope

-- TODO: Move to the Fx module.
instance MonadParallel (Fx env err) where
  bindM2 f l r =
    join $ concurrently $ \lift ->
      liftA2 f (lift l) (lift r)
