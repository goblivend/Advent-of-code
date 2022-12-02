module Main where
import Data.List

-- Get each line and creates a list of values
-- (list of the sums of snacks)
-- sets of values separated by blank lines
getList :: [String] -> Int -> [Int]
getList ("":l) acc = acc : (getList l 0)
getList (e:l) acc = getList l (acc + read e)
getList _ acc = acc :[]

main :: IO ()
main = do
    content <- readFile "input.txt"
    let input = lines content

    let res = getList input 0

    -- First exercise : Max
    print $ maximum res

    -- Second one, sum of the 3 tops
    print $ sum $ take 3 $ reverse $ sort res
