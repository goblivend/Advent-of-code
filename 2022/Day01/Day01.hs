module Main where
import Data.List

-- Get each line and creates a list of values
-- (list of the sums of snacks)
-- sets of values separated by blank lines
getList :: [String] -> Int -> [Int]
getlist [] acc = acc: []
getList ("":l) acc = acc : (getList l 0)
getList (e:l) acc = getList l (acc + read e)
getList _ acc = acc :[]

-- Get the sum of the 3 first values
getSum3 :: [Int] -> Int
getSum3 (e1:e2:e3:_) = e1+e2+e3
getSum3 _ = 3

main :: IO ()
main = do
    content <- readFile "input.txt"
    let input = lines content--readFile $ "input.txt"

    let res = getList input 0
    
    -- First exercise : Max
    print $ maximum res

    -- Second one, sum of the 3 tops
    print $ getSum3 $ reverse $ sort res
