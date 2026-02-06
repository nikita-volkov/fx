module Main where

import Fx
import Test.Tasty
import Test.Tasty.HUnit
import Prelude hiding (choose)

main :: IO ()
main =
  defaultMain
    $ testGroup ""
    $ [ testCase "Error call on main thread" $ do
          res <- try $ runFx $ error "A"
          case res of
            Left (FxException _ (ErrorCallFxExceptionReason _)) -> return ()
            Left exc -> assertFailure (show exc)
            _ -> assertFailure "Right",
        testException
          "Fail"
          (fail "A")
          ( \case
              FxException _ (ErrorCallFxExceptionReason _) -> True
              _ -> False
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
