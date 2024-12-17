module Main where

import Data.Bits
import Data.Char
import Data.List
import Data.List.Split
import Data.List.Unique
import Data.Matrix (Matrix, (!))
import Data.Matrix qualified as Mat
import Data.Set (Set)
import Data.Set qualified as S
import Data.Tuple.Extra
import Debug.Trace
import GHC.Integer (shiftRInteger)
import GHC.Num (integerFromInt, integerToInt)
import System.Environment
import Text.Regex.TDFA ((=~))

-- TODO: Cleanup imports after day done

data Input = Computer {program :: [Integer], programCounter :: Int, registerA :: Integer, registerB :: Integer, registerC :: Integer, output :: [Integer]} deriving (Show, Eq, Ord)

type Output = Integer

parseInput :: String -> Input
parseInput input = Computer pg 0 regA regB regC []
  where
    [lRegA, lRegB, lRegC, _, lPg] = lines input
    readVal = read . dropWhile (not . isDigit)
    regA = readVal lRegA
    regB = readVal lRegB
    regC = readVal lRegC
    pg = read . (\l -> "[" ++ l ++ "]") . dropWhile (not . isDigit) $ lPg

getValue :: Input -> Integer -> Integer
getValue (Computer _ _ rA _ _ _) 4 = rA
getValue (Computer _ _ _ rB _ _) 5 = rB
getValue (Computer _ _ _ _ rC _) 6 = rC
getValue (Computer _ _ _ _ _ _) n = n

calculateOp :: Input -> Integer -> Integer -> Input
calculateOp (c@(Computer _ pc rA rB rC out)) 0 n = c {programCounter = pc + 2, registerA = rA `div` (2 ^ (getValue c n))}
calculateOp (c@(Computer _ pc rA rB rC out)) 1 n = c {programCounter = pc + 2, registerB = rB `xor` n}
calculateOp (c@(Computer _ pc rA rB rC out)) 2 n = c {programCounter = pc + 2, registerB = (getValue c n) `mod` 8}
calculateOp (c@(Computer _ pc 00 rB rC out)) 3 n = c {programCounter = pc + 2}
calculateOp (c@(Computer _ pc rA rB rC out)) 3 n = c {programCounter = integerToInt $ getValue c n}
calculateOp (c@(Computer _ pc rA rB rC out)) 4 n = c {programCounter = pc + 2, registerB = rB `xor` rC}
calculateOp (c@(Computer _ pc rA rB rC out)) 5 n = c {programCounter = pc + 2, output = (getValue c n) `mod` 8 : out}
calculateOp (c@(Computer _ pc rA rB rC out)) 6 n = c {programCounter = pc + 2, registerB = rA `div` (2 ^ (getValue c n))}
calculateOp (c@(Computer _ pc rA rB rC out)) 7 n = c {programCounter = pc + 2, registerC = rA `div` (2 ^ (getValue c n))}

performOps :: Input -> [Output]
performOps (c@(Computer pg pc _ _ _ out))
  | pc >= length pg = reverse out
  | otherwise = performOps $ calculateOp c op comboOp
  where
    op = pg !! pc
    comboOp = pg !! (pc + 1)

part1 :: Input -> [Output]
part1 = performOps

part2 :: Input -> Output
part2 input = head $ sub' 1 0
  where
    sub' p v
      | (== p) . length $ program input = currIt
      | otherwise = concat $ map (sub' (p + 1)) currIt
      where
        currIt = filter (\i -> (`isSuffixOf` program input) . performOps $ input {registerA = i}) [8 * v .. 8 * v + 7]

main :: IO ()
main = do
  args <- getArgs
  content <- readFile (last args)
  let input = parseInput content
  print input

  print $ part1 input
  print $ part2 input
