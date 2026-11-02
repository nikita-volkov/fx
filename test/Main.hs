module Main where

import Fx
import Prelude hiding (choose)
import Test.Hspec

main :: IO ()
main = hspec do
  it "Error call on main thread" do
    res <- try $ runFx $ error "A"
    case res of
      Left (FxException _ (ErrorCallFxExceptionReason _) _) -> return ()
      Left exc -> expectationFailure (show exc)
      _ -> expectationFailure "Right"

  it "Fail" do
    testException
      (fail "A")
      ( \case
          FxException _ (ErrorCallFxExceptionReason _) _ -> True
          _ -> False
      )

  it "Racing completes with first result" do
    -- Test that racing returns the first result
    let slowAction = do
          -- Wrap in exception handler to catch ThreadKilled
          runTotalIO $ \_ ->
            catch
              (threadDelay 1000000) -- 1 second
              ( \case
                  ThreadKilled -> return () -- Just return on kill, we will check the kill in the test
                  exc -> throwIO exc
              )
          return (1 :: Int)

    let fastAction = do
          return (2 :: Int)

    -- Race them using concurrently
    result <- runFx $ concurrently $ \lift ->
      lift slowAction <|> lift fastAction

    -- The fast action should win
    result `shouldBe` 2

  it "Racing kills the losing thread" do
    -- Test that the losing thread is actually killed
    completedRef <- newIORef False
    started <- newEmptyMVar
    killed <- newEmptyMVar

    let slowAction = do
          -- This will be killed before it completes
          runTotalIO $ \_ -> do
            catch
              (putMVar started () >> threadDelay 5000000 >> writeIORef completedRef True)
              ( \case
                  ThreadKilled -> putMVar killed ()
                  exc -> throwIO exc
              )
          return (1 :: Int)

    let fastAction = do
          runTotalIO $ \_ -> readMVar started >> return (2 :: Int)

    -- Race them using concurrently
    result <- runFx $ concurrently $ \lift ->
      lift slowAction <|> lift fastAction

    -- The fast action should win
    result `shouldBe` 2

    killedByTimeout <- timeout 1000000 (takeMVar killed)
    killedByTimeout `shouldBe` Just ()

    -- The slow action should NOT have completed normally
    completed <- readIORef completedRef
    completed `shouldBe` False

testException :: Fx () Void a -> (FxException -> Bool) -> IO ()
testException fx validateExc = do
  res <- try @FxException $ runFx $ fx
  case res of
    Left exc ->
      if validateExc exc
        then return ()
        else expectationFailure (show exc)
    Right _ -> expectationFailure "No exception"
