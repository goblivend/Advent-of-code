module Main where

import Data.Set (Set)
import Data.Set qualified as S
import Debug.Trace
import System.Environment

type Input = [Int]

type Output = Int

parseInput :: String -> Input
parseInput = map (\c -> read [c]) . filter (/= '\n')

expand :: [Int] -> [Int]
expand = concat . map (\(i, n) -> replicate n (if i `mod` 2 == 0 then (i `div` 2) else -1)) . zip [0 ..]

exchangeAt :: [a] -> Int -> a -> [a]
exchangeAt arr i e = elsBefore ++ [e] ++ elsAfter
  where
    (elsBefore, _ : elsAfter) = splitAt i arr

retract :: [Int] -> [Int]
retract arr = sub arr (reverse arr) 0 (length arr - 1)
  where
    sub arr rev i j
      | i == j = [head arr]
      | head arr /= -1 = head arr : sub (drop 1 arr) rev (i + 1) j
      | head rev == -1 = sub arr (drop 1 rev) i (j - 1)
      | otherwise = head rev : sub (drop 1 arr) (drop 1 rev) (i + 1) (j - 1)

part1 :: Input -> Int
part1 = sum . map (uncurry (*)) . filter ((/= -1) . snd) . zip [0 ..] . retract . expand

expand2 :: [Int] -> [(Int, Int)]
expand2 = map (\(i, n) -> (if i `mod` 2 == 0 then (i `div` 2) else -1, n)) . zip [0 ..]

expandStr :: [(Int, Int)] -> String
expandStr = concat . map (\d -> if d == (-1) then "." else show d) . concat . map (\(i, n) -> replicate n i)

retract2 :: [(Int, Int)] -> [(Int, Int)]
retract2 arr = {-# SCC sub2 #-} sub2 arr (reverse arr)
  where
    subAdded :: [(Int, Int)] -> (Int, Int) -> [(Int, Int)]
    subAdded arr (idi, size)
      | idi == currId = (-1, size) : (drop 1 arr)
      | otherwise = head arr : subAdded (drop 1 arr) (idi, size)
      where
        (currId, currSize) = head arr

    sub :: [(Int, Int)] -> (Int, Int) -> [(Int, Int)]
    sub arr (idi, size)
      | idi == currId = arr
      | currId /= -1 = head arr : resSkip1
      | currId == -1 && nextId == -1 = {-# SCC subMergeMin1 #-} sub ((-1, currSize + nextSize) : arr) (idi, size)
      | currSize < size = head arr : resSkip1
      | currSize == size = (idi, size) : resExactMatch
      | currSize > size = (idi, size) : (-1, currSize - size) : resExactMatch
      where
        resSkip1 = {-# SCC nextEmptyFromSub #-} sub (drop 1 arr) (idi, size)
        (resExactMatch) = {-# SCC subAdded #-} subAdded (drop 1 arr) (idi, size)
        (currId, currSize) = head arr
        (nextId, nextSize) = head $ tail arr

    sub2 :: [(Int, Int)] -> [(Int, Int)] -> [(Int, Int)]
    sub2 arr rev
      | rev == [] = arr
      | (fst $ head rev) == -1 = {-# SCC sub2OnMin1 #-} sub2 arr (drop 1 rev)
      | otherwise = {-# SCC sub2Next #-} sub2 (newArr) (drop 1 rev)
      where
        newArr = {-# SCC sub2CallsSub #-} sub (arr) (head rev)
        currId = fst $ head rev

part2 :: Input -> Output
part2 = sum . map (uncurry (*)) . filter ((/= -1) . snd) . zip [0 ..] . concat . map (\(i, n) -> replicate n i) . retract2 . expand2

-- exchangeAt :: [a] -> Int -> a -> [a]
-- exchangeAt arr i e = elsBefore ++ [e] ++ elsAfter
--   where
-- (elsBefore, _:elsAfter) = splitAt i arr

-- Former Version : Was faster to do but is way slower
-- Quite similar to other version except that I run using [Int] hence time to look for the sizes and loop through all those numbers
-- retract2 :: [Int] -> [Int]
-- retract2 arr = {-# SCC sub2 #-} sub2 arr (reverse arr) (length arr -1)
--     where
--         sub arr jBlock i j
--             | i >= j = arr
--             | head arr /= -1 = iBlock ++ {-# SCC notFreeSubCall #-} sub (drop leni arr) jBlock (i+leni) j
--             | leni < lenj = iBlock ++ {-# SCC tooSmallSubCall #-} sub (drop leni arr) jBlock (i+leni) j
--             | otherwise = jBlock ++ {-# SCC removeElt #-} removeElt (head jBlock) (drop lenj arr)
--                 where
--                     iBlock = takeWhile (== (head arr)) arr
--                     leni = length iBlock
--                     lenj = length jBlock

--                     removeElt elt arr = map (\e -> if e == elt then -1 else e) arr

--         sub2 arr rev j
--             | j <= 0 = arr
--             | head rev == -1 = sub2 arr (drop lenj rev) (j - lenj)
--             | otherwise = sub2 ({-# SCC firstSubCall #-} sub arr jBlock 0 (j-lenj)) (drop lenj rev) (j - lenj)
--                 where
--                     jBlock = takeWhile (== (head rev)) rev
--                     lenj = length jBlock

-- part2 :: Input -> Output
-- part2 =  sum . map (uncurry (*)) . filter ((/= -1) . snd) . zip [0..] . retract2 . expand

main :: IO ()
main = do
  args <- getArgs
  content <- readFile (last args)
  let input = parseInput content

  print $ part1 input
  print $ part2 input
