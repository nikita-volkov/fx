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

  it "Racing cancels the loser" do
    -- Create two computations that would run forever if not cancelled
    -- Use IORefs to track whether they completed
    completedRef1 <- newIORef False
    completedRef2 <- newIORef False
    
    let slowAction1 = do
          runTotalIO $ \_ -> threadDelay 1000000 -- 1 second
          runTotalIO $ \_ -> writeIORef completedRef1 True
          return (1 :: Int)
    
    let fastAction2 = do
          runTotalIO $ \_ -> writeIORef completedRef2 True
          return (2 :: Int)
    
    -- Race them using concurrently
    result <- runFx $ concurrently $ \lift ->
      lift slowAction1 <|> lift fastAction2
    
    -- The fast action should win
    result `shouldBe` 2
    
    -- The slow action should NOT have completed
    completed1 <- readIORef completedRef1
    completed2 <- readIORef completedRef2
    completed1 `shouldBe` False
    completed2 `shouldBe` True

testException :: Fx () Void a -> (FxException -> Bool) -> IO ()
testException fx validateExc = do
  res <- try @FxException $ runFx $ fx
  case res of
    Left exc ->
      if validateExc exc
        then return ()
        else expectationFailure (show exc)
    Right _ -> expectationFailure "No exception"
