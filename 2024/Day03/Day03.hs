module Main where

import Data.List
import Data.List.Split
import Debug.Trace
import Text.Regex.TDFA ((=~))

data OpType = Mul | Do | Dont deriving (Show, Eq)

data Op = Op {opType :: OpType, left :: Int, right :: Int} deriving (Show)

type Input = [Op]

type Output = Int

parseInput :: String -> [Op]
parseInput s = map parseOp regex
  where
    regex = s =~ "(do)\\(\\)|(don't)\\(\\)|(mul)\\(([0-9]*),([0-9]*)\\)" :: [[String]]

    parseOp :: [String] -> Op
    parseOp [_, _, _, "mul", a, b] = Op Mul (read a) (read b)
    parseOp [_, "do", _, _, _, _] = Op Do (0) (0)
    parseOp [_, _, "don't", _, _, _] = Op Dont (0) (0)
    parseOp x = error ("Unknown op: " ++ show x)

calculate :: Input -> Output
calculate = fst . foldl fold (0, True)
  where
    fold (res, cond) op
      | opType op == Do = (res, True)
      | opType op == Dont = (res, False)
      | cond && opType op == Mul = (res + left op * right op, True)
      | otherwise = (res, cond)

part1 :: Input -> Output
part1 = calculate . filter ((== Mul) . opType)

part2 :: Input -> Output
part2 = calculate

main :: IO ()
main = do
  content <- readFile "input.txt"
  let input = parseInput content
  --   print input

  print $ part1 input
  print $ part2 input
