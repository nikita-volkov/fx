module Acquire
(
  Acquire(..),
  with,
)
where

import Acquire.Prelude


{-|
Implementation of http://www.haskellforall.com/2013/06/the-resource-applicative.html
-}
newtype Acquire resource =
  Acquire (IO (resource, IO ()))

instance Functor Acquire where
  fmap f (Acquire io) =
    Acquire $ do
      (resource, release) <- io
      return (f resource, release)

instance Applicative Acquire where
  pure resource =
    Acquire (pure (resource, pure ()))
  Acquire io1 <*> Acquire io2 =
    Acquire $ do
      (f, release1) <- io1
      (x, release2) <- onException io2 release1
      return (f x, release2 >> release1)

instance Monad Acquire where
  return = pure
  (>>=) (Acquire io1) k2 =
    Acquire $ do
      (resource1, release1) <- io1
      (resource2, release2) <- case k2 resource1 of Acquire io2 -> onException io2 release1
      return (resource2, release2 >> release1)

instance MonadIO Acquire where
  liftIO io =
    Acquire (fmap (, return ()) io)

with :: Acquire resource -> (resource -> IO a) -> IO a
with (Acquire io) handle =
  do
    (resource, release) <- io
    finally (handle resource) release
