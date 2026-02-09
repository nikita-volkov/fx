-- |
-- Future abstraction for concurrent computations
module Fx.Future
  ( -- * Future
    Future (..),
    mapFuture,
    start,
    wait,
    raceWithCancellation,
  )
where

import Fx.Fx
import Fx.Prelude
import qualified Fx.Strings as Strings

-- |
-- Handle to a result of an action which may still be being executed on another thread.
data Future err res = Future
  { futureThreadId :: ThreadId,
    futureResult :: Compose STM (Either (Maybe err)) res
  }

instance Functor (Future err) where
  fmap f (Future tid result) = Future tid (fmap f result)

instance Applicative (Future err) where
  pure a = error "Future: Cannot create pure Future without ThreadId"
  Future tid1 resultF <*> Future tid2 resultA =
    Future tid1 (resultF <*> resultA)

instance Alternative (Future err) where
  empty = error "Future: Cannot create empty Future without ThreadId"
  Future tid1 result1 <|> Future tid2 result2 =
    -- When one completes, cancel the other
    Future tid1 (result1 <|> result2)

instance Bifunctor Future where
  bimap lf rf = mapFuture (mapCompose (fmap (bimap (fmap lf) rf)))

mapFuture ::
  ( Compose STM (Either (Maybe err1)) res1 ->
    Compose STM (Either (Maybe err2)) res2
  ) ->
  Future err1 res1 ->
  Future err2 res2
mapFuture fn (Future tid m) = Future tid (fn m)

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

        tid <- forkIO $ do
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

        return $ Future tid $ Compose $ readTMVar futureVar

-- |
-- Block until the future completes either with a result or an error.
wait :: Future err res -> Fx env err res
wait (Future _ m) = Fx $
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

-- |
-- Race two futures, cancelling the loser when one completes.
-- Returns the result of the winner.
raceWithCancellation :: Future err res -> Future err res -> Future err res
raceWithCancellation (Future tid1 result1) (Future tid2 result2) =
  -- We create a virtual future that represents the race
  -- The actual cancellation happens after we know which won
  -- We'll use a TVar to track which thread won
  Future tid1 $ Compose $ do
    -- Use orElse to race - the first one to complete wins
    (getCompose result1 <* (unsafeIOToSTM (killThread tid2)))
      <|> (getCompose result2 <* (unsafeIOToSTM (killThread tid1)))
