module Acquire.IO
where

import Acquire.Prelude
import Acquire.Acquire


acquire :: Acquire resource -> (resource -> IO a) -> IO a
acquire (Acquire io) handle =
  do
    (resource, release) <- io
    finally (handle resource) release
