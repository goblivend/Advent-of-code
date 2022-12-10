module Main where

readInput :: String -> [Int]
readInput input
    | input == "noop" = [0]
    | otherwise = [0, read (inp !! 1) :: Int]
    where inp = words input

getStrength :: [Int] -> Int -> Int -> [Int] -> Int
getStrength _ _ _ [] = 0
getStrength [] _ _ _ = 0
getStrength (cycle:cycles) curr acc (e:l)
    | curr == cycle = cycle * acc + getStrength cycles (curr+1) (acc+e) l
    | otherwise = getStrength (cycle:cycles) (curr+1) (acc+e) l

drawCrt :: Int -> Int -> [Int] -> String
drawCrt curr acc [] = ""
drawCrt curr acc (e:l) = pixel ++ drawCrt (curr+1) (acc+e) l
    where
        color = if elem (mod curr 40) [acc-1..acc+1] then "#" else "."
        pixel = if mod curr 40 == 39 then color++"\n" else color



main :: IO ()
main = do
    content <- readFile "input.txt"
    let input = concat $ map readInput $ lines content
    print $ getStrength (map (\i -> 40*i+20) [0..5]) 1 1 input
    putStrLn $ drawCrt 0 1 input
