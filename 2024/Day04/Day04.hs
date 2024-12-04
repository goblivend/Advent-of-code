module Main where

import Data.Bits
import Data.List
import Data.List.Split
import Data.Matrix (Matrix, (!))
import Data.Matrix qualified as Mat
import Data.Tuple.Extra
import Debug.Trace
import Text.Regex.TDFA ((=~))

type Input = Matrix Char

type Output = Int

part1 :: Input -> Int
part1 grid = length . filter id . map (iterations "XMAS" grid) $ concat [[(x, y, dx, dy) | dx <- [-1 .. 1], dy <- [-1 .. 1], dx /= 0 || dy /= 0] | x <- [1 .. Mat.ncols grid], y <- [1 .. Mat.nrows grid]]
  where
    iterations :: String -> Matrix Char -> (Int, Int, Int, Int) -> Bool
    iterations [] _ _ = True
    iterations (l : w) grid (x, y, dx, dy)
      | x > Mat.ncols grid || y > Mat.nrows grid = False
      | x <= 0 || y <= 0 = False
      | l /= grid ! (x, y) = False
      | otherwise = iterations w grid (x + dx, y + dy, dx, dy)

part2 :: Input -> Output
part2 grid = length . filter id . map (\(x, y, cross) -> iterations grid (x, y) cross (1, 1)) $ [(x, y, cross) | x <- [1 .. Mat.ncols grid], y <- [1 .. Mat.nrows grid], cross <- crosses]
  where
    revert :: Matrix a -> Matrix a
    revert = Mat.fromLists . reverse . Mat.toLists

    crossXmas = Mat.fromLists ["M M", " A ", "S S"]
    crosses =
      [ crossXmas,
        Mat.transpose crossXmas,
        revert crossXmas,
        Mat.transpose $ revert crossXmas
      ]

    iterations :: Matrix Char -> (Int, Int) -> Matrix Char -> (Int, Int) -> Bool
    iterations grid xy xmas (xmasX, xmasY)
      | xmasX > Mat.ncols xmas = iterations grid xy xmas (1, xmasY + 1)
      | xmasY > Mat.nrows xmas = True
      | xmas ! (xmasX, xmasY) == ' ' = iterations grid xy xmas (xmasX + 1, xmasY)
    iterations grid (x, y) xmas (xmasX, xmasY)
      | (x + xmasX - 1) > Mat.ncols grid || (y + xmasY - 1) > Mat.nrows grid = False
      | xmas ! (xmasX, xmasY) /= grid ! (xmasX + x - 1, xmasY + y - 1) = False
      | otherwise = iterations grid (x, y) xmas (xmasX + 1, xmasY)

--

main :: IO ()
main = do
  content <- readFile "input.txt"
  let input = Mat.fromLists $ lines content

  print $ part1 $ input
  print $ part2 $ input
