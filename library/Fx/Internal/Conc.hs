-- |
-- Concurrent computations with Alternative and Applicative composition
module Fx.Internal.Conc
  ( -- * Conc
    concurrently,
  )
where

import Fx.Prelude
import Fx.Internal.Types
import Fx.Internal.Future (start, wait)

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
    wait (future1 <|> future2)

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
