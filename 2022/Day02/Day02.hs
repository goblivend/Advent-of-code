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

getResults :: (Int, Int) -> Int
getResults (a, b) = b + if a == b then 3
                        else if (mod (b-1)  3) == a || (mod (a+1)  3) == b  then 6
                        else 0

cleanInputs :: Int -> Char -> (Int, Int)
cleanInputs a b = if (b == 'X') then if a-1 == 0 then (a, 3) else (a, a-1)
                        else if (b == 'Y') then (a, a)
                        else if a+1 == 4 then (a, 1) else (a,  a+1)



main :: IO ()
main = do
    content <- readFile "input.txt"
    let plays =  map (\(a,b) -> (oponentPoints a, inputPoints b)) $ getPlays $ lines content
    putStrLn $ id "Following the first rules, the final score is :"
    print $ sum $ map (\(a,b) -> getResults(a, b)) $  plays

    let plays2 = map (\(a, b) -> (cleanInputs (oponentPoints a) b)) $ getPlays $ lines content
    putStrLn $ id "Following the second rules, the final score is :"
    print $ sum $  map (\(a,b) -> getResults(a, b)) $  plays2
