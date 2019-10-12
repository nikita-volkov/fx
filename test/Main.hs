module Main where

import Prelude hiding (choose)
import Test.QuickCheck.Instances
import Test.Tasty
import Test.Tasty.Runners
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import Fx
import qualified Test.QuickCheck as QuickCheck
import qualified Test.QuickCheck.Property as QuickCheck


main = defaultMain $ testGroup "" $
  [
    testCase "Error call on main thread" $ do
      res <- try $ runFx $ error "A"
      case res of
        Left (FxException _ (ErrorCallFxExceptionReason _)) -> return ()
        Left exc -> assertFailure (show exc)
        _ -> assertFailure "Right"
    ,
    testCase "Error call on forked thread" $ do
      res <- try $ runFx $ do
        future <- start $ error "A"
        wait future
      case res of
        Left (FxException _ (ErrorCallFxExceptionReason _)) -> return ()
        Left exc -> assertFailure (show exc)
        _ -> assertFailure "Right"
    ,
    testCase "Error call on deeply forked thread" $ do
      res <- try $ runFx $ do
        future <- start $ start $ error "A"
        wait future >>= wait
      case res of
        Left (FxException _ (ErrorCallFxExceptionReason _)) -> return ()
        Left exc -> assertFailure (show exc)
        _ -> assertFailure "Right"
  ]
