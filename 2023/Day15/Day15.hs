module Main where

import Data.Char
import Data.List
import Data.List.Unique
import Data.List.Split
import Data.Maybe ( fromJust, isJust )
import Debug.Trace(trace)
import Data.Set (Set, member, fromList, toList)
import Data.Map (Map)
import Data.Tuple.Extra
import qualified Data.Set as S (insert, empty)
import qualified Data.Map as M (insert, empty, lookup, size)

type Input = [String]

parseInput :: String -> Input
parseInput = splitOn "," . concat . lines

hash :: String -> Int
hash str = _hash str 0
    where
        _hash    [] acc = acc
        _hash (c:s) acc = _hash s (((acc + ord c)*17) `rem` 256)

part1 :: Input -> Int
part1 = sum . map hash


putInBoxes :: Input -> Map Int [(String, Int)]
putInBoxes inp = _putInBoxes inp M.empty
    where
        changeInBox []       kv' = [kv']
        changeInBox (kv:box) kv'
            | fst kv == fst kv' = kv' : box
            | otherwise         = kv  : changeInBox box kv'

        putInside m h v
            | isJust box' = M.insert h (changeInBox box v) m
            | otherwise   = M.insert h ([v])               m
                where
                    box' = M.lookup h m
                    box  = fromJust box'

        removeInBox m h k
            | isJust box = M.insert h (filter ((/= k) . fst) $ fromJust box) m
            | otherwise  = m
            where box = M.lookup h m

        lensInBox lens m
            | op == "=" = putInside   m h (label, nb)
            | op == "-" = removeInBox m h label
            where
                label = takeWhile isAlpha lens
                h = hash label
                op = take 1 $ dropWhile isAlpha lens
                nb = read $ dropWhile (not . isNumber) lens

        _putInBoxes []    m = m
        _putInBoxes (s:l) m = _putInBoxes l (lensInBox s m)


focusingPower :: Map Int [(String, Int)] -> Int
focusingPower m = sum . map boxPower . map (\(i, b) -> (i, fromJust b)) . filter (isJust . snd) . zip [1..] $ map (`M.lookup` m) [0..255]
    where
        lensPower boxNb (lensNb, (_, lensValue)) = boxNb * lensNb * lensValue
        boxPower (boxNb, box) = sum . map (lensPower boxNb) $ zip [1..] box

part2 :: Input -> Int
part2  = focusingPower . putInBoxes

main :: IO ()
main = do
    content <- readFile "input.txt"
    let inp = parseInput content
    print $ 513643 == part1 inp
    print $ 265345 == part2 inp
