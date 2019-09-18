module Acquire.IO
where

import Acquire.Prelude
import Acquire.Eio


eio :: Eio Void res -> IO res
eio (Eio (ExceptT io)) = fmap (either absurd id) io
