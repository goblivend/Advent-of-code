module Main where

import Data.Maybe ( fromJust, isJust )
import Debug.Trace(trace)
import Data.Set (Set, member, fromList, toList)
import Data.Map (Map, (!))
import qualified Data.Set as S
import qualified Data.Map as M
import Data.Complex

data Input = Input { area :: (Int, Int), grid::[[Int]]} deriving (Show)
data Direction = North | South | East | West deriving (Eq, Ord, Show)

instance Ord (Complex Int) where
  compare (a :+ b) (c :+ d) = compare (a, b) (c, d)

parseInput :: String -> Input
parseInput s = Input (length (grid !! 0), length grid) grid
    where
        grid = map (map (read . (:""))) . lines $ s

bfs :: (Int, Int) -> Input -> [Set (Complex Int, Direction, Int)] -> Set (Complex Int, Direction, Int) -> Int
bfs (mini, maxi) (Input (w, h) _) (l:arr) seen
    | ((w-1) :+ (h-1)) `member` (S.map (\(p, _, _) -> p) $ S.filter (\(_, _, n) -> n >= mini) l) = 0
bfs (mini, maxi) inp@(Input (w, h) grid) (l:arr) seen  = 1 + (bfs (mini, maxi) inp cleanedArr newSeen)
    where
        cleanedArr = map ((`S.difference` newSeen)) $ (foldl insertNext arr l)
        newSeen = seen `S.union` l
        add c1 c2 = (realPart c1 + realPart c2) :+ (imagPart c1 + imagPart c2)
        isInValid xy = realPart xy < 0 || w <= realPart xy || imagPart xy < 0 || h <= imagPart xy
        at xy = grid !! imagPart xy !! realPart xy
        dxy = M.fromList [(North, (0 :+ (-1))), (South, (0 :+ 1)), (East, (1 :+ 0)), (West, ((-1) :+ 0))]
        othersOf = M.fromList [(North, [East, West]), (South, [East, West]), (East, [North, South]), (West, [North, South])]
        insertAt arr e@(xy', _, _)
            | length arr >= at xy' = take (at xy'-1) arr ++ [(e `S.insert` (head $ drop (at xy'-1) arr))] ++ (drop (at xy') arr)
            | otherwise            = arr ++ (take (at xy' - length arr - 1) $ cycle [S.empty]) ++ [S.singleton e]
        others xy dir = filter (\(p, _, _) -> not $ isInValid p) $ map (\dir' -> (add xy (dxy ! dir'), dir', 1)) $ othersOf ! dir

        insertNext arr e@(xy, dir, n)
            | e `S.member` seen = arr
            | isInValid xy = (arr)
            | n < mini &&     (isInValid (add xy (dxy ! dir))) = arr
            | n < mini && not (isInValid (add xy (dxy ! dir))) =                 insertAt arr (add xy (dxy ! dir), dir, n+1)
            | n < maxi && not (isInValid (add xy (dxy ! dir))) = foldl insertAt (insertAt arr (add xy (dxy ! dir), dir, n+1)) $ others xy dir
            | n >= maxi ||     isInValid (add xy (dxy ! dir))  = foldl insertAt arr                                           $ others xy dir
            | otherwise                                        = arr

part1 :: Input -> Int
part1 inp = bfs (0, 3) inp [S.fromList [((0 :+ 0), South, 0)]] S.empty

part2 :: Input -> Int
part2 inp = bfs (4, 10) inp [S.fromList [((0 :+ 0), South, 0), ((0 :+ 0), East, 0)]] S.empty

main :: IO ()
main = do
    content <- readFile "input.txt"
    let inp = parseInput content
    print $ part1 inp
    print $ part2 inp
