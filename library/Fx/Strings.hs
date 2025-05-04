module Fx.Strings where

import Fx.Prelude

failedWaitingForFinalResult :: SomeException -> String
failedWaitingForFinalResult exc =
  showString "Failed waiting for final result: " (show exc)

failedWaitingForResult :: SomeException -> String
failedWaitingForResult exc =
  showString "Failed waiting for result: " (show exc)

unexpectedException :: SomeException -> String
unexpectedException exc =
  showString "Unexpected exception: " (show exc)

uncaughtException :: SomeException -> String
uncaughtException exc =
  showString "Uncaught exception: " (show exc)

fatalErrorAtThreadPath :: [ThreadId] -> String -> String
fatalErrorAtThreadPath =
  let showTids = intercalate "/" . fmap (drop 9 . show)
   in \tids reason ->
        showString ("Fatal error at thread path /")
          $ showString (showTids tids)
          $ showString ". "
          $ reason

bug :: String -> String
bug details =
  showString "Bug in the \"fx\" library. Please report it to maintainers. " details
