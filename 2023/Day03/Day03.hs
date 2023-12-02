module Main where

import Data.Char
import Data.List
import Data.List.Split

type Coordinate = (Int, Int)

data Symbol = Gear | Other | Void deriving (Eq)

type SymbolsLine = [Symbol]

type Symbols = [SymbolsLine]

readSymbol :: Char -> Symbol
readSymbol '*' = Gear
readSymbol _ = Other

readSymbols :: String -> SymbolsLine
readSymbols [] = []
readSymbols ('.' : s) = Void : readSymbols s
readSymbols (c : s)
  | isDigit c = Void : readSymbols s
  | otherwise = (readSymbol c) : readSymbols s

hasSymbol :: Symbols -> Coordinate -> Bool
hasSymbol symbols (x, y) = Void /= ((symbols !! y) !! x)

hasSideSymbol :: Symbols -> Int -> Int -> Bool
hasSideSymbol symbols y x = foldr (||) False $ map (hasSymbol symbols) sides
  where
    clamp (nx, ny) = 0 <= ny && ny < length symbols && 0 <= nx && nx < length (symbols !! 0)
    sides = filter (clamp) [(x + dx, y + dy) | dx <- [-1 .. 1], dy <- [-1 .. 1]]

part1 :: Symbols -> Int -> (Int, String) -> [Int]
part1 symbols x (y, "") = []
part1 symbols x (y, s@(c : r))
  | isDigit c = n : part1 symbols (x + nbc) (y, drop nbc s)
  | otherwise = part1 symbols (x + 1) (y, r)
  where
    number = takeWhile isDigit s
    nbc = length number
    isCounted = foldr (||) False $ map (hasSideSymbol symbols y) [x .. x + nbc - 1]
    n = if isCounted then (read number) else 0

coFold :: Coordinate -> [Coordinate] -> [Coordinate]
coFold (-1, -1) l = l
coFold e l
  | elem e l = l
  | otherwise = e : l

hasGear :: Symbols -> Coordinate -> Bool
hasGear symbols (x, y) = Gear == ((symbols !! y) !! x)

gearCoordinate :: Symbols -> Coordinate -> Coordinate
gearCoordinate symbols (x, y)
  | hasGear symbols (x, y) = (x, y)
  | otherwise = (-1, -1)

sideGearCoordinate :: Symbols -> Int -> Int -> [Coordinate]
sideGearCoordinate symbols y x = foldr (coFold) [] $ map (gearCoordinate symbols) sides
  where
    clamp (nx, ny) = 0 <= ny && ny < length symbols && 0 <= nx && nx < length (symbols !! 0)
    sides = filter (clamp) [(x + dx, y + dy) | dx <- [-1 .. 1], dy <- [-1 .. 1]]

part2 :: Symbols -> Int -> (Int, String) -> [((Int, Int), Int)]
part2 symbols x (y, "") = []
part2 symbols x (y, s@(c : r))
  | isDigit c = coos ++ part2 symbols (x + nbc) (y, drop nbc s)
  | otherwise = part2 symbols (x + 1) (y, r)
  where
    number = takeWhile isDigit s
    nbc = length number
    gearCo = foldr (coFold) [] $ concat $ map (sideGearCoordinate symbols y) [x .. x + nbc - 1]
    coos = map (\co -> (co, read number)) gearCo

main :: IO ()
main = do
  content <- readFile "input.txt"
  let input = lines content
  let symbols = map readSymbols input
  print $ sum $ concat $ map (part1 symbols 0) $ zip [0 ..] input
  print $ sum $ map (foldr (*) 1 . map (\e -> snd e)) $ filter (\l -> length l >= 2)$ groupBy (\e1 e2 -> fst e1 == fst e2) $ sort $ concat $ map (part2 symbols 0) $ zip [0 ..] input
