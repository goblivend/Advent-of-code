module Main where

import Data.List
import Data.Map (Map)
import Data.Map qualified as M
import Data.Tuple.Extra
import System.Environment

type Input = [(Int, Int)]

type Output = Int

parseInput :: String -> Input
parseInput = map (read . (++ ")") . ("(" ++)) . lines

pairs :: [a] -> [(a, a)]
pairs l = [(x, y) | (x : l) <- tails l, y <- l]

size :: (Num a) => (a, a) -> (a, a) -> a
size (x, y) (x', y') = (abs (x - x') + 1) * (abs (y - y') + 1)

part1 :: Input -> Output
part1 = maximum . map (uncurry size) . pairs

inSqr :: (Ord a) => ((a, a), (a, a)) -> (a, a) -> Bool
inSqr ((x, y), (x', y')) (x'', y'') = x < x'' && x'' < x' && y < y'' && y'' < y'

limitsOf :: (Ord a, Ord b) => ((a, b), (a, b)) -> ((a, b), (a, b))
-- limitsOf ((x, y), (x', y')) = ((min x x', min y y'), (max x x', max y y'))
limitsOf ((x, y), (x', y')) -- Not really nice looking version but much faster
  | x' < x = limitsOf ((x', y), (x, y'))
  | y' < y = ((x, y'), (x', y))
  | otherwise = ((x, y), (x', y'))

inCol :: (Ord a) => ((a, a), (a, a)) -> (a, a) -> Bool
inCol ((x, y), (x', y')) (x'', y'') = x'' == x && y < y'' && y'' < y'

inRow :: (Ord a) => ((a, a), (a, a)) -> (a, a) -> Bool
inRow ((x, y), (x', y')) (x'', y'') = y'' == y && x < x'' && x'' < x'

part2 :: Input -> Output
part2 redTiles = uncurry size . head . filter (uncurry limitNbSides . limitsOf) $ filter noneDirectlyIn sortedPairs --
  where
    greenLines = map limitsOf $ zip redTiles (tail redTiles ++ [head redTiles])

    greenCols = M.fromAscList . map (\((xy, xy') : l) -> (fst xy, (xy, xy') : l)) . groupBy (curry (uncurry (==) . both fst)) . sortOn (fst . fst) $ filter (uncurry (==) . both fst) greenLines
    greenRows = M.fromAscList . map (\((xy, xy') : l) -> (snd xy, (xy, xy') : l)) . groupBy (curry (uncurry (==) . both snd)) . sortOn (snd . fst) $ filter (uncurry (==) . both snd) greenLines

    limitNbSides :: (Int, Int) -> (Int, Int) -> Bool
    limitNbSides (x, y) (x', y') = not $ any (uncurry (hitSides (x, y) (x', y'))) [(((x + x') `div` 2, y + 1), (0, 1)), ((x + 1, (y + y') `div` 2), (1, 0))]

    hitSides :: (Int, Int) -> (Int, Int) -> (Int, Int) -> (Int, Int) -> Bool
    hitSides (x, y) (x', y') (x'', y'') dxy
      | dxy == (0, 1) && (y'' == y') = False
      | dxy == (1, 0) && (x'' == x') = False
      | dxy == (1, 0) && M.member x'' greenCols && any (flip inCol (x'', y'')) (greenCols M.! x'') = True
      | dxy == (0, 1) && M.member y'' greenRows && any (flip inRow (x'', y'')) (greenRows M.! y'') = True
      | otherwise = hitSides (x, y) (x', y') (x'' + fst dxy, y'' + snd dxy) dxy

    noneDirectlyIn xyxy' = not $ any (inSqr (limitsOf xyxy')) redTiles
    sortedPairs = reverse . sortOn (uncurry size) $ pairs redTiles

main :: IO ()
main = do
  args <- getArgs
  content <- readFile (last args)
  let input = parseInput content

  -- print input

  print $ 4741451444 == part1 input
  print $ 1562459680 == part2 input
