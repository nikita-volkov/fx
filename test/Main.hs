module Main where

import Fx
import Test.Hspec
import Prelude hiding (choose)

main :: IO ()
main = hspec do
  it "Error call on main thread" do
    res <- try $ runFx $ error "A"
    case res of
      Left (FxException _ (ErrorCallFxExceptionReason _)) -> return ()
      Left exc -> expectationFailure (show exc)
      _ -> expectationFailure "Right"

  it "Fail" do
    testException
      (fail "A")
      ( \case
          FxException _ (ErrorCallFxExceptionReason _) -> True
          _ -> False
      )

  it "Racing completes with first result" do
    -- Test that racing returns the first result
    let slowAction = do
          runTotalIO $ \_ -> threadDelay 1000000 -- 1 second
          return (1 :: Int)
    
    let fastAction = do
          return (2 :: Int)
    
    -- Race them using concurrently
    result <- runFx $ concurrently $ \lift ->
      lift slowAction <|> lift fastAction
    
    -- The fast action should win
    result `shouldBe` 2

testException :: Fx () Void a -> (FxException -> Bool) -> IO ()
testException fx validateExc = do
  res <- try @FxException $ runFx $ fx
  case res of
    Left exc ->
      if validateExc exc
        then return ()
        else expectationFailure (show exc)
    Right _ -> expectationFailure "No exception"
