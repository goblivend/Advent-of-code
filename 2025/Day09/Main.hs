module Day09.Main (day09) where

import AOC.Cli (defaultInput, getP1P2Flags, orDefault, usage)
import AOC.Image (upScale)
import AOC.Runtime (printRuntime, printRuntimeNoRes)
import AOC.Utils (pairs)
import Codec.Picture -- (Image, PixelRGB8, generateImage, writePng)
import Control.Monad (when)
import Data.List (groupBy, sortOn)
import Data.List.Extra (maximumOn)
import Data.Map (Map)
import Data.Map qualified as M (fromAscList, member, (!))
import Data.Set qualified as S (fromList, member)
import Data.Tuple.Extra (both, dupe, second, (&&&))
import System.Console.GetOpt (ArgDescr (..), ArgOrder (..), OptDescr (..), getOpt)

type Input = [(Int, Int)]

type Output = Int

parseInput :: String -> Input
parseInput = map (read . (++ ")") . ("(" ++)) . lines

size :: (Num a) => (a, a) -> (a, a) -> a
size (x, y) (x', y') = (abs (x - x') + 1) * (abs (y - y') + 1)

part1' :: Input -> ((Int, Int), (Int, Int))
part1' = fst . maximumOn snd . map (second (uncurry size) . dupe) . pairs

part1 :: Input -> Output
part1 = uncurry size . part1'

inSqr :: (Ord a) => ((a, a), (a, a)) -> (a, a) -> Bool
inSqr ((x, y), (x', y')) (x'', y'') = x < x'' && x'' < x' && y < y'' && y'' < y'

limitsOf :: (Ord a, Ord b) => ((a, b), (a, b)) -> ((a, b), (a, b))
-- limitsOf ((x, y), (x', y')) = ((min x x', min y y'), (max x x', max y y'))
limitsOf ((x, y), (x', y')) -- Not really nice looking version but much faster
  | x' < x = limitsOf ((x', y), (x, y'))
  | y' < y = ((x, y'), (x', y))
  | otherwise = ((x, y), (x', y'))

groupLinesOn :: ((Int, Int) -> Int) -> [((Int, Int), (Int, Int))] -> Map Int [((Int, Int), (Int, Int))]
groupLinesOn f = M.fromAscList . map (\((xy, xy') : l) -> (f xy, (xy, xy') : l)) . groupBy (curry (uncurry (==) . both (f . snd))) . sortOn (f . fst) . filter (uncurry (==) . both f)

getLines :: [(Int, Int)] -> (Map Int [((Int, Int), (Int, Int))], Map Int [((Int, Int), (Int, Int))])
getLines tiles = (&&&) (groupLinesOn fst) (groupLinesOn snd) . map limitsOf $ zip tiles (tail tiles ++ [head tiles])

inCol :: (Ord a) => ((a, a), (a, a)) -> (a, a) -> Bool
inCol ((x, y), (_, y')) (x'', y'') = x'' == x && y < y'' && y'' < y'

inRow :: (Ord a) => ((a, a), (a, a)) -> (a, a) -> Bool
inRow ((x, y), (x', _)) (x'', y'') = y'' == y && x < x'' && x'' < x'

inIt :: (a -> b -> Bool) -> (b -> Int) -> Map Int [a] -> b -> Bool
inIt f f' m xy = M.member (f' xy) m && any (flip f xy) (m M.! (f' xy))

part2' :: Input -> ((Int, Int), (Int, Int))
part2' redTiles = head . filter (uncurry limitNbSides . limitsOf) $ filter noneDirectlyIn sortedPairs --
  where
    (greenCols, greenRows) = getLines redTiles

    limitNbSides :: (Int, Int) -> (Int, Int) -> Bool
    limitNbSides (x, y) (x', y') = not $ any (uncurry (hitSides (x', y'))) [(((x + x') `div` 2, y + 1), (0, 1)), ((x + 1, (y + y') `div` 2), (1, 0))]

    hitSides :: (Int, Int) -> (Int, Int) -> (Int, Int) -> Bool
    hitSides (x', y') (x'', y'') dxy
      | dxy == (0, 1) && (y'' == y') = False
      | dxy == (1, 0) && (x'' == x') = False
      | dxy == (1, 0) && inIt inCol fst greenCols (x'', y'') = True
      | dxy == (0, 1) && inIt inRow snd greenRows (x'', y'') = True
      | otherwise = hitSides (x', y') (x'' + fst dxy, y'' + snd dxy) dxy

    noneDirectlyIn xyxy' = not $ any (inSqr (limitsOf xyxy')) redTiles
    sortedPairs = reverse . sortOn (uncurry size) $ pairs redTiles

part2 :: Input -> Output
part2 = uncurry size . part2'

blankPixel :: PixelRGB8
blankPixel = PixelRGB8 0 0 0

redPixel :: PixelRGB8
redPixel = PixelRGB8 255 0 0

greenPixel :: PixelRGB8
greenPixel = PixelRGB8 0 255 0

p1Pixel :: PixelRGB8
p1Pixel = PixelRGB8 0 255 255

p2Pixel :: PixelRGB8
p2Pixel = PixelRGB8 255 0 255

inLine :: Map Int [((Int, Int), (Int, Int))] -> Map Int [((Int, Int), (Int, Int))] -> (Int, Int) -> Bool
inLine greenCols greenRows xy = inIt inRow snd greenRows xy || inIt inCol fst greenCols xy

inSqrBrdr :: ((Int, Int), (Int, Int)) -> (Int, Int) -> Bool
inSqrBrdr ((bx, by), (bx', by')) (x, y)
  | x `elem` [bx, bx'] = by <= y && y <= by'
  | y `elem` [by, by'] = bx <= x && x <= bx'
  | otherwise = False

visualize :: Int -> Int -> Input -> Image PixelRGB8
visualize dsf pad redTiles = generateImage go w h -- dsf == DownScaleFactor
  where
    newTiles = map (both (`div` dsf)) redTiles
    minX = minimum $ map fst newTiles
    minY = minimum $ map snd newTiles
    maxX = maximum $ map fst newTiles
    maxY = maximum $ map snd newTiles

    (dx, dy) = (minX - pad, minY - pad)
    (w, h) = (maxX - minX + 1 + 2 * pad, maxY - minY + 1 + 2 * pad)

    go x' y'
      | xy `S.member` reds = redPixel
      | inLine greenCols greenRows xy = greenPixel
      | inSqrBrdr p1 xy = p1Pixel
      | inSqrBrdr p2 xy = p2Pixel
      | otherwise = blankPixel
      where
        xy = (x' + dx, y' + dy)
    p1 = both (both (`div` dsf)) . limitsOf $ part1' redTiles
    p2 = both (both (`div` dsf)) . limitsOf $ part2' redTiles
    reds = S.fromList newTiles

    (greenCols, greenRows) = getLines newTiles

data Flag = Input String | Help | Part1 | Part2 | Visualize | Output String | Upscale Int | Dsf Int | Padding Int
  deriving (Eq, Show)

options :: [OptDescr Flag]
options =
  [ Option ['i'] ["input"] (ReqArg Input "FILE") "Input File (default to : input.txt)",
    Option ['h'] ["help"] (NoArg Help) "Display the help message",
    Option [] ["p1"] (NoArg Part1) "Calculate part 1",
    Option [] ["p2"] (NoArg Part2) "Calculate part 2",
    Option ['v'] ["visualize"] (NoArg Visualize) "Visualize puzzles results",
    Option ['o'] ["output"] (ReqArg Output "FILE") "Output File for visualization (default to : output.png)",
    Option ['d'] ["dsf"] (ReqArg (Dsf . read) "N") "Down scale factor to apply (default to : 256)",
    Option ['p'] ["pad"] (ReqArg (Padding . read) "N") "Padding to apply on the image",
    Option ['u'] ["upscale"] (ReqArg (Upscale . read) "N") "Upscale to apply on the final image"
  ]

day09 :: [String] -> IO ()
day09 args = do
  let (flags, _, _) = getOpt (ReturnInOrder Input) options args
  let help = Help `elem` flags
      input = orDefault (defaultInput 2025 09) [f | Input f <- flags]
      (p1, p2) = getP1P2Flags Part1 Part2 flags
      visu = Visualize `elem` flags
      output = orDefault "output.png" [f | Output f <- flags]
      up = orDefault 1 [n | Upscale n <- flags]
      dsf = orDefault 256 [n | Dsf n <- flags]
      pad = orDefault 0 [n | Padding n <- flags]

  if help
    then usage 2025 09 options
    else do
      content <- readFile input
      let inp = parseInput content

      when p1 $ printRuntime ((++) "2025/Day09 Part1: " . show) (return (part1 inp))
      when p2 $ printRuntime ((++) "2025/Day09 Part2: " . show) (return (part2 inp))
      when visu $ printRuntimeNoRes ("2025/Day09 Wrote visualization to " ++ output) (writePng output $ upScale up $ visualize dsf pad inp)
