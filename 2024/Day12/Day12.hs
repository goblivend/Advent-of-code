module Main where

import Data.List
import Data.Matrix (Matrix, (!))
import Data.Matrix qualified as Mat
import Data.Set (Set)
import Data.Set qualified as S
import Data.Tuple.Extra
import Debug.Trace
import System.Environment
import Text.Regex.TDFA ((=~))

type Input = Matrix Char

type Output = Int

data Dir = UP | LEFT | RIGHT | DOWN deriving (Show, Eq, Ord)

parseInput :: String -> Input
parseInput = Mat.fromLists . lines

indices :: Matrix a -> [(Int, Int)]
indices grid = [(y, x) | y <- [1 .. Mat.nrows grid], x <- [1 .. Mat.ncols grid]]

move :: (Int, Int) -> Dir -> (Int, Int)
move (y, x) UP = (y - 1, x)
move (y, x) DOWN = (y + 1, x)
move (y, x) LEFT = (y, x - 1)
move (y, x) RIGHT = (y, x + 1)

isOut :: (Int, Int) -> (Int, Int) -> Bool
isOut (width, height) (y, x) = x <= 0 || y <= 0 || x > width || y > height

mapOutRegion :: Matrix Char -> Char -> (Int, Int) -> Dir -> Set (Int, Int) -> (Set (Int, Int), Set ((Int, Int), Dir))
mapOutRegion grid plant yx dir seen
  | yx `S.member` seen = (seen, S.empty)
  | isOut (Mat.ncols grid, Mat.nrows grid) yx = (seen, S.singleton (yx, dir))
  | grid ! yx /= plant = (seen, S.singleton (yx, dir))
  | otherwise = foldl (\res dir' -> second (S.union (snd res)) $ mapOutRegion grid plant (move yx dir') dir' (fst res)) (S.insert yx seen, S.empty) [UP, LEFT, RIGHT, DOWN]

algo :: (Set ((Int, Int), Dir) -> Int) -> Input -> Output
algo wallCounter grid = sum . fst . foldl folder ([], S.empty) $ indices grid
  where
    folder :: ([Int], Set (Int, Int)) -> (Int, Int) -> ([Int], Set (Int, Int))
    folder (prices, seen) yx
      | yx `S.member` seen = (prices, seen)
      | otherwise = ((S.size iteSeen * wallCounter wallSeen) : prices, S.union iteSeen seen)
      where
        (iteSeen, wallSeen) = mapOutRegion grid (grid ! yx) yx UP S.empty

part1 :: Input -> Output
part1 = algo S.size

nbSides :: Set ((Int, Int), Dir) -> Int
nbSides walls = sum $ map (uncurry nbWallLength) groupedWallLst
  where
    wallLst :: [((Int, Int), Dir)]
    wallLst = S.toList walls

    sidedWallLst :: [[((Int, Int), Dir)]]
    sidedWallLst = groupBy (\a b -> snd a == snd b) $ sortOn snd wallLst

    groupedWallLst :: [(Dir, [(Int, Int)])]
    groupedWallLst = map (\arr -> (snd $ head arr, map fst arr)) sidedWallLst

    nbWallLength UP = nbOn . sort
    nbWallLength DOWN = nbOn . sort
    nbWallLength LEFT = nbOn . sort . map swap
    nbWallLength RIGHT = nbOn . sort . map swap

    nbOn = snd . foldl folder ((-1, -1), 0)
      where
        folder (lastYX, nbWalls) yx
          | fst yx /= fst lastYX = (yx, nbWalls + 1)
          | snd yx /= snd lastYX + 1 = (yx, nbWalls + 1)
          | otherwise = (yx, nbWalls)

part2 :: Input -> Output
part2 = algo nbSides

main :: IO ()
main = do
  args <- getArgs
  content <- readFile (last args)
  let input = parseInput content

  -- print $ Mat.toLists input

  print $ (== 1465112) $ part1 input
  print $ (== 893790) $ part2 input
