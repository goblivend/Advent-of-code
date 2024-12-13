module Main where

import Data.Bits
import Data.Either
import Data.List
import Data.List.Split
import Data.List.Unique
import Data.Matrix (Matrix, (!))
import Data.Matrix qualified as Mat
import Data.Set (Set)
import Data.Set qualified as S
import Data.Tuple.Extra
import Debug.Trace
import System.Environment
import Text.Regex.TDFA ((=~))

-- TODO: Cleanup imports after day done

type Input = [((Double, Double), (Double, Double), (Double, Double))]

type Output = Int

parseInput :: String -> Input
parseInput input = res
  where
    res = map (\[sb1, sb2, sprize] -> (getPos sb1, getPos sb2, getPos sprize)) . map lines $ splitOn "\n\n" input

    t :: String -> [[String]]
    t s = s =~ ".* X.(.*), Y.(.*)" :: [[String]]
    getPos :: String -> (Double, Double)
    getPos s = (read x, read y)
      where
        [[_, x, y]] = t s

part1 :: Input -> Output
part1 = sum . map (\(nx, ny) -> 3 * nx + ny) . map fst . filter snd . map (uncurry3 solveArcade)
  where
    -- Old version checking for valid combinations => Waayyyy too sloww for part2
    -- solveArcade pos seen b1 b2 prize
    --   | pos == prize = ((0, 0), (seen, True))
    --   | pos `S.member` seen = ((-1, -1), (seen, False))
    --   | fst pos >= fst prize = ((-1, -1), (S.insert pos seen, False))
    --   | snd pos >= snd prize = ((-1, -1), (S.insert pos seen, False))
    --   -- | trace (show (pos, prize)) (False) = ((0, 0), False)
    --   | not succced1 && not succced2 = ((-1, -1), (S.insert pos seen2, False))
    --   | succced1 && not succced2 = (res1, (S.insert pos seen2, True))
    --   | not succced1 && succced2 = (res2, (S.insert pos seen2, True))
    --   | fst res1 * 3 + snd res1 < fst res2 * 3 + snd res2 = (res1, (S.insert pos seen2, True))
    --   | otherwise = (res1, (S.insert pos seen2, True))
    --     where
    --       (res1, (seen1, succced1)) = first (first ((+)1)) $ solveArcade (fst pos + fst b1, snd pos + snd b1) seen b1 b2 prize
    --       (res2, (seen2, succced2)) = first (second ((+)1)) $ solveArcade (fst pos + fst b2, snd pos + snd b2) seen1 b1 b2 prize

    isAlmostInt :: Double -> Bool
    isAlmostInt x = abs (x - fromInteger (round x)) < eps
      where
        eps = 1e-2 --- REALLY sketchy : doubles are not precise enough to have a good eps (1e-10 or something close) so had to guess it and get 1e-2 since 1e-1 is too big and 1e-4 is too small
    solveArcade (xb1, yb1) (xb2, yb2) (xp, yp)
      | isLeft inva = trace (show (fromLeft "" inva)) ((-1, -1), False)
      | isAlmostInt nb1 && isAlmostInt nb2 = ((round nb1, round nb2), True)
      | otherwise = ((-1, -1), False)
      where
        a :: Matrix Double
        a = Mat.fromLists [[xb1, xb2], [yb1, yb2]]
        b :: Matrix Double
        b = Mat.fromLists [[xp], [yp]]
        inva :: Either String (Matrix Double)
        inva = (Mat.inverse a)
        res :: Matrix Double
        res = Mat.multStd (fromRight (Mat.zero 1 1) inva) b
        (nb1, nb2) = (res ! (1, 1), res ! (2, 1))

part2 :: Input -> Output
part2 = part1 . map (\(b1, b2, p) -> (b1, b2, both ((+) 10000000000000) p))

main :: IO ()
main = do
  args <- getArgs
  content <- readFile (last args)
  let input = parseInput content

  -- print input

  print $ part1 input
  print $ part2 input
