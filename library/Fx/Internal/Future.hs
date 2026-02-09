-- |
-- Future abstraction for concurrent computations
module Fx.Internal.Future
  ( -- * Future
    start,
    wait,
  )
where

import Fx.Prelude
import qualified Fx.Strings as Strings
import Fx.Internal.Types

-- |
-- Spawn a thread and start running an effect on it,
-- returning the associated future.
--
-- Fatal errors on the spawned thread are guaranteed to get propagated to the top.
-- By fatal errors we mean calls to `error`, `fail` and uncaught exceptions.
--
-- Normal errors (the explicit @err@ parameter) will only propagate
-- if you use `wait` at some point.
--
-- __Warning:__
-- It is your responsibility to ensure that the whole future executes
-- before the running `Fx` finishes.
-- Otherwise you will lose the environment in scope of which the future executes.
-- To achieve that use `wait`.
start :: Fx env err res -> Fx env err' (Future err res)
start (Fx m) =
  Fx $
    ReaderT $
      \(FxEnv unmask crash env) -> lift $ do
        futureVar <- newEmptyTMVarIO

        _ <- forkIO $ do
          tid <- myThreadId

          let childCrash tids dls = crash (tid : tids) dls

          finalize <-
            catch
              ( do
                  res <- runExceptT (runReaderT m (FxEnv unmask childCrash env))
                  return (atomically (putTMVar futureVar (first Just res)))
              )
              ( \exc -> return $ do
                  case fromException exc of
                    -- Catch calls to `error`.
                    Just errorCall -> crash [] (ErrorCallFxExceptionReason errorCall)
                    -- Catch anything else we could miss. Just in case.
                    _ -> crash [] (BugFxExceptionReason (Strings.unexpectedException exc))
                  atomically (putTMVar futureVar (Left Nothing))
              )

          finalize

        return $ Future $ Compose $ readTMVar futureVar

-- |
-- Block until the future completes either with a result or an error.
wait :: Future err res -> Fx env err res
wait (Future m) = Fx $
  ReaderT $
    \(FxEnv unmask crash _) ->
      ExceptT $
        join $
          catch
            ( do
                futureStatus <- unmask (atomically (getCompose m))
                return $ case futureStatus of
                  Right res -> return (Right res)
                  Left (Just err) -> return (Left err)
                  Left Nothing -> fail "Waiting for a future that crashed"
            )
            ( \(exc :: SomeException) -> return $ do
                crash [] (BugFxExceptionReason (Strings.failedWaitingForResult exc))
                fail "Thread crashed with uncaught exception waiting for result."
            )
