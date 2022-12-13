module Main where

import Data.Char
import Data.List
import Data.List.Split
import Data.Containers.ListUtils
import Debug.Trace

parseLine :: String -> [Int]
parseLine [] = []
parseLine (e:l)
    | e == ',' = parseLine l
    | e == '[' = -1:parseLine l
    | e == ']' = -2:parseLine l
    | otherwise = (fst (getInt (e:l))):parseLine (snd (getInt (e:l)))
        where
            getInt :: String -> (Int, String)
            getInt [] = (0, [])
            getInt (e:l)
                | isDigit e =  if isDigit (head l) then
                                ((read [e] :: Int) * 10 + (fst (getInt l)), (snd (getInt l)))
                                else
                                ((read [e] :: Int), l)
                | otherwise = (0, e:l)


isOrdered :: [Int] -> [Int] -> Bool
isOrdered [] _ = True
isOrdered _ [] = False
isOrdered (e1:l1) (e2:l2)
    | e1 == -2 && e2 == -2 = isOrdered l1 l2
    | e1 == -2 && e2 /= -2 = True
    | e1 /= -2 && e2 == -2 = False
    | e1 == -1 && e2 == -1 = isOrdered l1 l2
    | e1 == -1 && e2 /= -1 = isOrdered (e1:l1) ((-1):e2:(-2):l2)
    | e1 /= -1 && e2 == -1 = isOrdered ((-1):e1:(-2):l1) (e2:l2)
    | e1 == e2 = isOrdered l1 l2
    | otherwise = e1 < e2


main :: IO ()
main = do
    content <- readFile "input.txt"
    let lists = map parseLine $ filter (/=[]) $ lines content
    print $ sum $ map (\x -> if isOrdered (lists !! (2*x)) (lists !! (2*x+1)) then 1+x else 0) [0..div (length lists - 1)2]
    let newl = [-1,2,-2]:[-1,6,-2]:lists
    let ordered = sortBy (\x y -> if isOrdered x y then LT else GT) newl
    print $ product $ filter (\x -> ordered !! (x-1) == [-1,2,-2] || ordered !! (x-1) == [-1,6,-2]) [1..length ordered]
