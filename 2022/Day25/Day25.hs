module Main where

import Data.Char
import Data.List
import Data.List.Split
import Data.Containers.ListUtils
import Debug.Trace

digitSnafuToInt :: Char -> Int
digitSnafuToInt c 
    | isDigit c = read [c] :: Int
    | c == '-' = -1
    | c == '=' = -2

convSnafuToInt :: String -> Int
convSnafuToInt line = sum nbs
    where
        zipped = zip (reverse line) [0..]
        nbs = map (\(nb, exp) -> 5^exp * digitSnafuToInt nb) zipped

convIntToSnafu :: Int -> String
convIntToSnafu number 
    | number == 0 = []
    | number `mod` 5 <= 2 = convIntToSnafu (number `div` 5) ++ show (number `mod` 5)
    | number `mod` 5 == 3 = convIntToSnafu ((number + 2) `div` 5) ++ "="
    | number `mod` 5 == 4 = convIntToSnafu ((number + 1) `div` 5) ++ "-"

main :: IO ()
main = do
    content <- readFile "input.txt"
    let numbers = convSnafuToInt <$> lines content
    let total = sum numbers
    print total
    print $ convIntToSnafu total
    print $ convSnafuToInt $ convIntToSnafu total
