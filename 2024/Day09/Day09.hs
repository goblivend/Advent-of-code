module Main where
import Debug.Trace
import System.Environment

type Input = [Int]
type Output = Int

parseInput :: String -> Input
parseInput = map (\c -> read [c]) . filter (/= '\n')

expand :: [Int] -> [Int]
expand = concat . map (\(i, n) -> replicate n (if i `mod` 2 == 0 then (i `div` 2) else -1)) . zip [0..]

exchangeAt :: [a] -> Int -> a -> [a]
exchangeAt arr i e = elsBefore ++ [e] ++ elsAfter
  where
        (elsBefore, _:elsAfter) = splitAt i arr

retract :: [Int] -> [Int]
retract arr = sub arr (reverse arr) 0 (length arr - 1)
    where
        sub arr rev i j
            | i == j = [head arr]
            | head arr /= -1 = head arr : sub (drop 1 arr) rev (i+1) j
            | head rev == -1 = sub arr (drop 1 rev) i (j-1)
            | otherwise = head rev: sub (drop 1 arr) (drop 1 rev) (i+1) (j-1)


part1 :: Input -> Int
part1 =  sum . map (uncurry (*)) . zip [0..] . retract . expand


-- Artifact of previous version in which I segmented the drive as (id, size) (id -1 beeing free) but had the issue of [(-1, 2), (-1, 3)] which should be converted to [(-1, 5)]
-- and meant to pass onee more time on the array => seemed either slow or more complicated even if doable
-- expand2 :: [Int] -> [(Int, Int)]
-- expand2 = map (\(i, n) -> (if i `mod` 2 == 0 then (i `div` 2) else -1, n)) . zip [0..]

-- retract2 :: [(Int, Int)] -> [(Int, Int)]
-- retract2 arr = foldl (\res (j, (idi, size)) -> sub res 0 j (idi, size)) arr $ zip (reverse [0..length arr - 1]) arr
--     where
--         sub :: [(Int, Int)] -> Int -> Int -> (Int, Int) -> Bool -> [(Int, Int)]
--         sub arr i j (idi, size) added
--             | added && i == j = (-1, size) : (drop 1 arr)
--             | i >= j = arr
--             | idi == -1 = arr
--             | (fst $ head arr) /= -1 = head arr : sub (drop 1 arr) (i+1) j (idi, size) added
--             | (snd $ head arr) < size = head arr : sub (drop 1 arr) (i+1) j (idi, size) added
--             | (snd $ head arr) == size = (idi, size) : sub (drop 1 arr) (i+1) j (idi, size) True
--             | (snd $ head arr) > size = (idi, size) : (-1, (snd $ head arr) - size) : sub (drop 1 arr) (i+1) j (idi, size) True
--         sub2 arr rev j
--             | rev == [] = arr
--             | (fst $ head rev) == -1 = sub2 arr (drop 1 rev) (j-1)
--             | otherwise = sub arr 0 j head (rev)


-- exchangeAt :: [a] -> Int -> a -> [a]
-- exchangeAt arr i e = elsBefore ++ [e] ++ elsAfter
--   where
        -- (elsBefore, _:elsAfter) = splitAt i arr

retract2 :: [Int] -> [Int]
retract2 arr = {-# SCC sub2 #-} sub2 arr (reverse arr) (length arr -1)
    where
        sub arr jBlock i j
            | i >= j = arr
            | head arr /= -1 = iBlock ++ {-# SCC notFreeSubCall #-} sub (drop leni arr) jBlock (i+leni) j
            | leni < lenj = iBlock ++ {-# SCC tooSmallSubCall #-} sub (drop leni arr) jBlock (i+leni) j
            | otherwise = jBlock ++ {-# SCC removeElt #-} removeElt (head jBlock) (drop lenj arr)
                where
                    iBlock = takeWhile (== (head arr)) arr
                    leni = length iBlock
                    lenj = length jBlock

                    removeElt elt arr = map (\e -> if e == elt then -1 else e) arr

        sub2 arr rev j
            | j <= 0 = arr
            | head rev == -1 = sub2 arr (drop lenj rev) (j - lenj)
            | otherwise = sub2 ({-# SCC firstSubCall #-} sub arr jBlock 0 (j-lenj)) (drop lenj rev) (j - lenj)
                where
                    jBlock = takeWhile (== (head rev)) rev
                    lenj = length jBlock


part2 :: Input -> Output
part2 =  sum . map (uncurry (*)) . filter ((/= -1) . snd) . zip [0..] . retract2 . expand

main :: IO ()
main = do
  args  <- getArgs
  content <- readFile (last args)
  let input = parseInput content

  print $ part1 input
  print $ part2 input
