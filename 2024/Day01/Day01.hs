module Main where
import Data.List
import Text.Regex.TDFA ( (=~) )

readLine :: String -> (Int, Int)
readLine s = (read e1, read e2)
    where
        t = s =~ "(.*)   (.*)" :: [[String]]
        tuple = head t
        e1 = head . tail $ tuple
        e2 = head . tail . tail $ tuple

splitLists :: [(Int, Int)] -> ([Int], [Int])
splitLists = foldl (\e2 e1 -> ((fst e1):(fst e2), (snd e1):(snd e2))) ([], [])

part1 :: [Int] -> [Int] -> Int
part1 l1 l2 = sum $ map (\(e1, e2) -> abs (e1 - e2)) $ (zip sortedL1 sortedL2)
    where
        sortedL1 = sort l1
        sortedL2 = sort l2


part2 :: [Int] -> [Int] -> Int
part2 l1 l2 = sum $ score <$> l1
    where
        score e = e * (length $ filter (e==) l2)

main :: IO()
main = do
    content <- readFile "input.txt"
    let input = splitLists $ readLine <$> lines content
    print $ uncurry part1 input
    print $ uncurry part2 input
