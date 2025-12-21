module AOC2025 (run2025) where

import Day01.Main (day01)
import Day02.Main (day02)
import Day03.Main (day03)
import Day04.Main (day04)
import Day05.Main (day05)
import Day06.Main (day06)
import Day07.Main (day07)
import Day08.Main (day08)
import Day09.Main (day09)
import Day10.Main (day10)
import Day11.Main (day11)
import Day12.Main (day12)
import Text.Printf

days :: [[String] -> IO ()]
days =
  [ day01,
    day02,
    day03,
    day04,
    day05,
    day06,
    day07,
    day08,
    day09,
    day10,
    day11,
    day12
  ]

run2025 :: Int -> [String] -> IO ()
run2025 day opts
  | day == -1 = sequence_ $ ($ opts) <$> days
  | day > length days = error (printf "For year 2025, Day%d out of bounds [1,%d]" day (length days))
  | otherwise = (days !! (day - 1)) opts
