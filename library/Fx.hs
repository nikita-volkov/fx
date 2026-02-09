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
    releasing,

    -- * Exceptions
    FxException (..),
    FxExceptionReason (..),
  )
where

-- Re-export from internal modules
import Fx.Internal.Types (Fx, Scope, FxException (..), FxExceptionReason (..))
import Fx.Internal.Fx
import Fx.Internal.Scope
import Fx.Internal.Future
import Fx.Internal.Conc

