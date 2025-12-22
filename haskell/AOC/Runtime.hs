module AOC.Runtime (printRuntime, printRuntimeNoRes) where

import Text.Printf
import System.CPUTime


{- ---- FORMATTING ---- -}

formatTime :: Double -> String
formatTime seconds
  | seconds >= 60.0 = printf "%dmin%.0fs" minutes remainingSeconds
  | otherwise = printf "%.3fs" seconds
  where
    minutes = floor (seconds / 60.0) :: Int
    remainingSeconds = seconds - fromIntegral minutes * 60.0

{- ---- MEASURING ---- -}

measureRuntime :: (IO a) -> IO (Double, a)
measureRuntime action = do
  start <- getCPUTime
  let !startTime = start -- Forcing evaluation
  result <- action
  let !res = result -- Forcing evaluation
  end <- getCPUTime
  let !endTime = end -- Forcing evaluation
  let diff = fromIntegral (endTime - startTime) / (10 ^ (12 :: Int)) :: Double
  return (diff, res)

measureRuntimeNoRes :: (IO ()) -> IO Double
measureRuntimeNoRes action = do
  start <- getCPUTime
  let !startTime = start -- Forcing evaluation
  action
  end <- getCPUTime
  let !endTime = end -- Forcing evaluation
  let diff = fromIntegral (endTime - startTime) / (10 ^ (12 :: Int)) :: Double
  return diff

{- ---- PRINTING ---- -}

printRuntime :: (a -> String) -> IO a -> IO ()
printRuntime strBuilder action = do
  (time, res) <- measureRuntime action
  let resStr = strBuilder res
  putStrLn $ resStr ++ " in " ++ formatTime time

printRuntimeNoRes :: String -> IO () -> IO ()
printRuntimeNoRes str action = putStrLn . ((++) (str ++ " in ")) . formatTime =<< measureRuntimeNoRes action
