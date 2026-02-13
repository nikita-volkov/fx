module Main where

import Fx
import Test.Hspec
import Prelude hiding (choose)

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
    killedRef <- newIORef False

    let slowAction = do
          -- This will be killed before it completes
          runTotalIO $ \_ -> do
            -- Use catch to detect if thread is killed
            catch
              (threadDelay 5000000 >> writeIORef completedRef True) -- 5 seconds
              ( \case
                  ThreadKilled -> writeIORef killedRef True
                  _ -> pure ()
              )
          return (1 :: Int)

    let fastAction = do
          return (2 :: Int)

    -- Race them using concurrently
    result <- runFx $ concurrently $ \lift ->
      lift slowAction <|> lift fastAction

    -- The fast action should win
    result `shouldBe` 2

    -- Give a moment for the kill signal to be processed
    threadDelay 100000 -- 100ms

    -- The slow action should NOT have completed normally
    completed <- readIORef completedRef
    completed `shouldBe` False

    -- The slow action should have been killed
    killed <- readIORef killedRef
    killed `shouldBe` True

testException :: Fx () Void a -> (FxException -> Bool) -> IO ()
testException fx validateExc = do
  res <- try @FxException $ runFx $ fx
  case res of
    Left exc ->
      if validateExc exc
        then return ()
        else expectationFailure (show exc)
    Right _ -> expectationFailure "No exception"
