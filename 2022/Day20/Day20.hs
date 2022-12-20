module Main where

import Data.Char
import Data.List
import Data.List.Split
import Data.Containers.ListUtils
import Debug.Trace

data Array = Array { indeces :: [Int], values :: [Int], len :: Int } deriving (Eq, Ord, Read, Show)

slide :: Int -> Int -> Array -> Array
slide i newI ar = ar {indeces = newIndeces}
        where
            indx = indeces ar
            val = values ar
            (beginI, (e:endI)) = splitAt i indx
            (beginBefore, endBefore) = splitAt newI beginI
            (beginAfter, endAfter) = splitAt (newI - i) endI
            newIndeces = if newI < i then beginBefore ++ e:endBefore ++ endI else beginI ++ beginAfter ++ e:endAfter

reorder :: Array -> Int -> Array
reorder ar n
    | n >= len ar = ar
    | otherwise = reorder (slide i newI ar) (n+1)
        where
            i = head $ elemIndices n (indeces ar)
            e = (values ar) !! n
            newItemp = mod (e+i) (len ar - 1)
            newI = if newItemp == 0 then len ar - 1 else newItemp

getValuesOrdered :: [Int] -> [Int] ->[Int]
getValuesOrdered values indeces = map (values !!) indeces

getCos :: Array-> Int -> Int
getCos ar n
    | n <= 1 = newAr !! 1000 + newAr !! 2000 + newAr !! 3000
    | otherwise = getCos reordered (n-1)
    where
        reordered = reorder ar 0
        newAr = dropWhile (/=0) . cycle . getValuesOrdered (values ar) $ indeces reordered

main :: IO ()
main = do
    content <- readFile "input.txt"
    let array = (\l -> read l :: Int) <$> lines content
    let len = length array
    let ar = Array [0..len - 1] array len
    print $ getCos ar 1

    let array2 = map (*811589153) array
    let ar2 = Array [0..len - 1] array2 len
    print $ getCos ar2 10

    -- print $ getValuesOrdered (values ar) $ indeces $ reorder ar 0


-- 19559
-- 912226207972
