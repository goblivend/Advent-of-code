module Main where

getList :: [String] -> Int -> [Int]
getlist [] acc = acc: []
getList ("":l) acc = acc : (getList l 0)
getList (e:l) acc = getList l (acc + read e)



main :: IO ()
main = do
    content <- readFile "input.txt"
    let input = lines content--readFile $ "input.txt"

    let res = getList input 0

    print res

    print $ maximum res
