module AOC.Cli (orDefault, getDefaultFlags, defaultUsage, defaultUsageFinalDay, usage, defaultInput, getP1P2Flags, getDefaultFlagsFinalDay) where

import Data.Maybe
import System.Console.GetOpt
import Text.Printf

orDefault :: a -> [a] -> a
orDefault x l = fromMaybe x $ listToMaybe l

data DefaultFlag = Input String | Help | Part1 | Part2
  deriving (Eq, Show)

defaultOptions :: [OptDescr DefaultFlag]
defaultOptions =
  [ Option ['i'] ["input"] (ReqArg Input "FILE") "Input File (default to : input.txt)",
    Option ['h'] ["help"] (NoArg Help) "Display the help message",
    Option [] ["p1"] (NoArg Part1) "Calculate part 1",
    Option [] ["p2"] (NoArg Part2) "Calculate part 2"
  ]

defaultOptionsFinalDay :: [OptDescr DefaultFlag]
defaultOptionsFinalDay =
  [ Option ['i'] ["input"] (ReqArg Input "FILE") "Input File (default to : input.txt)",
    Option ['h'] ["help"] (NoArg Help) "Display the help message"
  ]

defaultInput :: Int -> Int -> String
defaultInput year day = printf "%d/Day%02d/input.txt" year day

getDefaultFlags :: Int -> Int -> [String] -> (Bool, String, Bool, Bool)
getDefaultFlags year day args = (help, input, both || p1, both || p2)
  where
    (flags, _, _) = getOpt (ReturnInOrder Input) defaultOptions args
    help = Help `elem` flags
    input = orDefault (defaultInput year day) [f | Input f <- flags]
    p1 = Part1 `elem` flags
    p2 = Part2 `elem` flags
    both = p1 == p2

getDefaultFlagsFinalDay :: Int -> Int -> [String] -> (Bool, String, Bool)
getDefaultFlagsFinalDay year day args = (help, input, True)
  where
    (flags, _, _) = getOpt (ReturnInOrder Input) defaultOptionsFinalDay args
    help = Help `elem` flags
    input = orDefault (defaultInput year day) [f | Input f <- flags]

defaultUsage :: Int -> Int -> IO ()
defaultUsage year day = usage year day defaultOptions

defaultUsageFinalDay :: Int -> Int -> IO ()
defaultUsageFinalDay year day = usage year day defaultOptionsFinalDay

usage :: Int -> Int -> [OptDescr a] -> IO ()
usage year day = putStrLn . usageInfo (printf "Usage: %d-day%d [OPTIONS]" year day)

getP1P2Flags :: (Eq a) => a -> a -> [a] -> (Bool, Bool)
getP1P2Flags part1 part2 flags = (bothp1p2 || p1, bothp1p2 || p2)
  where
    p1 = part1 `elem` flags
    p2 = part2 `elem` flags
    bothp1p2 = p1 == p2
