module Main where
import Data.List
import Data.Char

firstGetIntLine :: String -> [String]
firstGetIntLine [] = []
firstGetIntLine (e:s)
    | isDigit e = [e] : firstGetIntLine s
    | otherwise = firstGetIntLine s

-- Some digits are written down
secondGetIntLine :: String -> [String]
secondGetIntLine [] = []
secondGetIntLine l@(e:s)
    | isDigit e = [e] : secondGetIntLine s
    | isPrefixOf "one"   l = "1" : secondGetIntLine s--(drop 2 s)
    | isPrefixOf "two"   l = "2" : secondGetIntLine s--(drop 2 s)
    | isPrefixOf "three" l = "3" : secondGetIntLine s--(drop 4 s)
    | isPrefixOf "four"  l = "4" : secondGetIntLine s--(drop 3 s)
    | isPrefixOf "five"  l = "5" : secondGetIntLine s--(drop 3 s)
    | isPrefixOf "six"   l = "6" : secondGetIntLine s--(drop 2 s)
    | isPrefixOf "seven" l = "7" : secondGetIntLine s--(drop 4 s)
    | isPrefixOf "eight" l = "8" : secondGetIntLine s--(drop 4 s)
    | isPrefixOf "nine"  l = "9" : secondGetIntLine s--(drop 3 s)
    | otherwise = secondGetIntLine s


-- Get each line and creates a list of values
-- list of the concatenation of the first and last digit of the line
getList :: (String -> [String]) -> [String] -> [Int]
getList mapper [] = []
getList mapper (e:s) = number : getList mapper s
    where
        digits = mapper e
        strNumber = (head digits) ++ (last digits)
        number = read strNumber

main :: IO ()
main = do
    content <- readFile "input.txt"
    let input = lines content
    print $ sum $ getList (firstGetIntLine) input
    print $ sum $ getList (secondGetIntLine) input
    -- I don't think the expected result is the right one, here I would say it's only 8 but in this case we need the 2 too
    print $ secondGetIntLine "eightwo"
