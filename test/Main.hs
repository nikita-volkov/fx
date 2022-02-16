module Main where

import Fx
import qualified Test.QuickCheck as QuickCheck
import Test.QuickCheck.Instances
import qualified Test.QuickCheck.Property as QuickCheck
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import Test.Tasty.Runners
import Prelude hiding (choose)

main =
  defaultMain $
    testGroup "" $
      [ testCase "Error call on main thread" $ do
          res <- try $ runFx $ error "A"
          case res of
            Left (FxException _ (ErrorCallFxExceptionReason _)) -> return ()
            Left exc -> assertFailure (show exc)
            _ -> assertFailure "Right",
        testCase "Error call on forked thread" $ do
          res <- try $
            runFx $ do
              future <- start $ error "A"
              wait future
          case res of
            Left (FxException _ (ErrorCallFxExceptionReason _)) -> return ()
            Left exc -> assertFailure (show exc)
            _ -> assertFailure "Right",
        testCase "Error call on deeply forked thread" $ do
          res <- try $
            runFx $ do
              future <- start $ start $ error "A"
              wait future >>= wait
          case res of
            Left (FxException _ (ErrorCallFxExceptionReason _)) -> return ()
            Left exc -> assertFailure (show exc)
            _ -> assertFailure "Right",
        testException
          "Fail"
          (fail "A")
          ( \case
              FxException _ (ErrorCallFxExceptionReason _) -> True
              exc -> False
          ),
        testException
          "Fail on forked thread"
          (start (fail "A") >>= wait)
          ( \case
              FxException _ (ErrorCallFxExceptionReason _) -> True
              exc -> False
          )
      ]

testException :: TestName -> Fx () Void a -> (FxException -> Bool) -> TestTree
testException name fx validateExc = testCase name $ do
  res <- try @FxException $ runFx $ fx
  case res of
    Left exc ->
      if validateExc exc
        then return ()
        else assertFailure (show exc)
    Right _ -> assertFailure "No exception"
