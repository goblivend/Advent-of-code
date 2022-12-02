module Main where
import Data.List
import Data.Char


inputPoints :: Char -> Int
inputPoints c = (ord c) - (ord 'W')

oponentPoints :: Char -> Int
oponentPoints c = (ord c) - (ord 'A') + 1

getPlays :: [String] -> [(Char, Char)]
getPlays (e:l) = (e !! 0, e !! 2) : getPlays l
getPlays [] = []

getResults :: [(Int, Int)] -> Int
getResults [] = 0
getResults ((a, b):l) =b + (getResults l) + if a == b then 3
                        else if (mod (b-1)  3) == a || (mod (a+1)  3) == b  then 6
                        else 0



main :: IO ()
main = do
    content <- readFile "input.txt"
    let plays =  map (\(a,b) -> (oponentPoints a, inputPoints b)) $ getPlays $ lines content
    putStrLn $ id "Following the first rules, the final score is :"
    print $  getResults plays
