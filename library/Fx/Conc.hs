-- |
-- Concurrent computations with Alternative and Applicative composition
module Fx.Conc
  ( -- * Conc
    Conc (..),
    concurrently,
  )
where

import Fx.Future (Future (..), start, wait)
import Fx.Fx (Fx, RunsFx (..), runTotalIO)
import Fx.Prelude

-- |
-- Wrapper over `Fx`, whose instances compose by running computations on separate threads.
newtype Conc env err res = Conc (Fx env err res)

deriving instance Functor (Conc env err)

deriving instance Bifunctor (Conc env)

instance Applicative (Conc env err) where
  pure = Conc . pure
  (<*>) (Conc m1) (Conc m2) = Conc $ do
    future1 <- start m1
    res2 <- m2
    res1 <- wait future1
    return (res1 res2)

instance Alternative (Conc env err) where
  empty = Conc do
    wait empty
  (<|>) (Conc m1) (Conc m2) = Conc $ do
    future1 <- start m1
    future2 <- start m2
    -- Race the futures - the first to complete wins
    result <- wait (future1 <|> future2)
    -- Cancel the loser thread
    -- Both threads get killed; the winner has already completed so killThread is a no-op
    case (futureThreadId future1, futureThreadId future2) of
      (Just tid1, Just tid2) -> runTotalIO $ \_ -> do
        void $ try @SomeException $ killThread tid1
        void $ try @SomeException $ killThread tid2
      _ -> return ()
    return result

-- |
-- Execute concurrent effects in either one or a combination of the following ways:
--
-- - __Complete__: Run in parallel and wait for all results (Applicative instance)
-- - __Race__: Run in parallel and choose the result of the first one to produce it or to fail (Alternative instance)
--
-- E.g.,
--
-- > selectDataById :: Int64 -> Fx env err (Metadata, File)
-- > selectDataById id =
-- >   concurrently $ \lift ->
-- >     (,)
-- >       <$> lift (selectMetadataById id)
-- >       <*> lift (getFileById id)
--
-- One interesting use of the `Alternative` instance is implementing timeouts:
--
-- > timeout :: Int -> Fx env err res -> Fx env err (Maybe res)
-- > timeout millis action =
-- >   concurrently $ \lift ->
-- >     lift action
-- >       <|> lift (runTotalIO (Nothing <$ threadDelay (fromIntegral millis * 1000))))
concurrently ::
  (forall f. (Alternative f) => (forall x. Fx env err x -> f x) -> f res) ->
  Fx env err res
concurrently build =
  case build Conc of
    Conc fx -> fx

instance RunsFx env err (Conc env err) where
  runFx = Conc
