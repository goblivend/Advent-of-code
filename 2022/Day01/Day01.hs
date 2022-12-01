module Main where

increments :: [Int] -> Int
increments l = length . filter (== True) . zipWith (<) l $ tail l

sumThree :: [Int] -> [Int]
sumThree l = zipWith3 (\x y z -> x + y + z) l (tail l) (drop 2 l)

getList :: [String] -> Int -> [Int]
getlist [] acc = acc: []
getList ("":l) acc = acc : (getList l 0)
getList (e:l) acc = getList l (acc + read e)


main :: IO ()
main = do
  content <- readFile "input.txt"
  let input = map lines $ content

  print input
  --print $ maximum (getList input 0)
  --print $ increments $ sumThree input
