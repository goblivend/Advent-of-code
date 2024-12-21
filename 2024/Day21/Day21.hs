module Main where

import Data.Bits
import Data.Char
import Data.List
import Data.List.Split
import Data.List.Unique
import Data.Map (Map)
import Data.Map qualified as M
import Data.Matrix (Matrix, (!))
import Data.Matrix qualified as Mat
import Data.Set (Set)
import Data.Set qualified as S
import Data.Tuple.Extra
import Debug.Trace
import System.Environment
import Text.Regex.TDFA ((=~))

-- TODO: Cleanup imports after day done

type Input = [String]

type Output = Int

parseInput :: String -> Input
parseInput = lines

shorten :: [String] -> [String]
shorten = head . groupBy (\e1 e2 -> length e1 == length e2) . sortOn length

shortenS :: Set String -> Set String
shortenS = S.fromList . head . groupBy (\e1 e2 -> length e1 == length e2) . sortOn length . S.toList

sConcat :: (Ord a) => Set (Set a) -> Set a
sConcat = S.unions . S.toList

memoise2 :: (Ord a, Ord b) => Map (a, b) c -> (a -> b -> c) -> a -> b -> (Map (a, b) c, c)
memoise2 mem f a b
  | (a, b) `M.member` mem = (mem, mem M.! (a, b))
  | otherwise = (M.insert (a, b) res mem, res)
  where
    res = f a b

memoRec2 :: (Ord a, Ord b) => Map (a, b) c -> (Map (a, b) c -> a -> b -> (Map (a, b) c, c)) -> a -> b -> (Map (a, b) c, c)
memoRec2 mem f a b
  | (a, b) `M.member` mem = (mem, mem M.! (a, b))
  | otherwise = (M.insert (a, b) res mem', res)
  where
    (mem', res) = f mem a b

memoise3 :: (Ord a, Ord b, Ord c) => Map (a, b, c) d -> (a -> b -> c -> d) -> a -> b -> c -> (Map (a, b, c) d, d)
memoise3 mem f a b c
  | (a, b, c) `M.member` mem = (mem, mem M.! (a, b, c))
  | otherwise = (M.insert (a, b, c) res mem, res)
  where
    res = f a b c

goToY :: [(Int, Int)] -> (Int, Int) -> (Int, Int) -> String
goToY possible (x, y) (x', y')
  | x == x' && y == y' = []
  | y < y' && (x, y + 1) `elem` possible = 'v' : goToY possible (x, y + 1) (x', y')
  | y > y' && (x, y - 1) `elem` possible = '^' : goToY possible (x, y - 1) (x', y')
  | x < x' && (x + 1, y) `elem` possible = '>' : goToY possible (x + 1, y) (x', y')
  | x > x' && (x - 1, y) `elem` possible = '<' : goToY possible (x - 1, y) (x', y')

goToX :: [(Int, Int)] -> (Int, Int) -> (Int, Int) -> String
goToX possible (x, y) (x', y')
  | x == x' && y == y' = []
  | x < x' && (x + 1, y) `elem` possible = '>' : goToX possible (x + 1, y) (x', y')
  | x > x' && (x - 1, y) `elem` possible = '<' : goToX possible (x - 1, y) (x', y')
  | y < y' && (x, y + 1) `elem` possible = 'v' : goToX possible (x, y + 1) (x', y')
  | y > y' && (x, y - 1) `elem` possible = '^' : goToX possible (x, y - 1) (x', y')

isValid :: [(Int, Int)] -> (Int, Int) -> String -> Bool
isValid validSpots xy [] = xy `elem` validSpots
isValid validSpots (x, y) ('v' : s) = (x, y) `elem` validSpots && isValid validSpots (x, y + 1) s
isValid validSpots (x, y) ('^' : s) = (x, y) `elem` validSpots && isValid validSpots (x, y - 1) s
isValid validSpots (x, y) ('>' : s) = (x, y) `elem` validSpots && isValid validSpots (x + 1, y) s
isValid validSpots (x, y) ('<' : s) = (x, y) `elem` validSpots && isValid validSpots (x - 1, y) s

generalEmulator :: Map (Int, Int) Char -> (Int, Int) -> String -> String
generalEmulator poss posA code = sub code posA
  where
    sub [] _ = []
    sub ('A' : l) xy = poss M.! xy : sub l xy
    sub ('v' : l) (x, y) = sub l (x, y + 1)
    sub ('^' : l) (x, y) = sub l (x, y - 1)
    sub ('>' : l) (x, y) = sub l (x + 1, y)
    sub ('<' : l) (x, y) = sub l (x - 1, y)

numericalEmulator :: String -> String
numericalEmulator code = generalEmulator numMap (2, 3) code
  where
    numMap =
      M.fromList $
        [ ((0, 0), '7'),
          ((1, 0), '8'),
          ((2, 0), '9'),
          ((0, 1), '4'),
          ((1, 1), '5'),
          ((2, 1), '6'),
          ((0, 2), '1'),
          ((1, 2), '2'),
          ((2, 2), '3'),
          ((1, 3), '0'),
          ((2, 3), 'A')
        ]

directionalEmulator :: String -> String
directionalEmulator code = generalEmulator dirMap (2, 0) code
  where
    dirMap = M.fromList $ [((1, 0), '^'), ((2, 0), 'A'), ((0, 1), '<'), ((1, 1), 'v'), ((2, 1), '>')]

sequencerC :: [(Int, Int)] -> (Int, Int) -> (Int, Int) -> Set String
sequencerC valid xy' xy = S.map (++ "A") goTos
  where
    goTos = S.fromList $ shorten . map (\gt -> gt valid xy xy') $ [goToX, goToY]

sequencer :: Map Char (Int, Int) -> String -> (Int, Int) -> Set String
sequencer poss [] _ = S.singleton []
sequencer poss (e : l) xy = sConcat $ S.map (\r -> S.map (++ r) goTos) $ nextRes
  where
    xy' = (poss M.! e)
    nextRes = sequencer poss l xy'
    goTos = sequencerC (M.elems poss) xy' xy

numericalSequencer :: String -> Set String
numericalSequencer code = sequencer numMap code (numMap M.! 'A')
  where
    numMap =
      M.fromList $
        [ ('7', (0, 0)),
          ('8', (1, 0)),
          ('9', (2, 0)),
          ('4', (0, 1)),
          ('5', (1, 1)),
          ('6', (2, 1)),
          ('1', (0, 2)),
          ('2', (1, 2)),
          ('3', (2, 2)),
          ('0', (1, 3)),
          ('A', (2, 3))
        ]

directionalSequencer :: String -> Set String
directionalSequencer code = sequencer dirMap code (dirMap M.! 'A')
  where
    dirMap = M.fromList $ [('^', (1, 0)), ('A', (2, 0)), ('<', (0, 1)), ('v', (1, 1)), ('>', (2, 1))]

depthMemorySequencer :: Map ((Int, Int), ((Int, Int), Int)) Int -> Int -> String -> (Map ((Int, Int), ((Int, Int), Int)) Int, Int)
depthMemorySequencer mem depth code = sub mem depth code
  where
    dirMap = M.fromList $ [('^', (1, 0)), ('A', (2, 0)), ('<', (0, 1)), ('v', (1, 1)), ('>', (2, 1))]
    minLength = length . head . sortOn length

    subc' :: Map ((Int, Int), ((Int, Int), Int)) Int -> (Int, Int) -> (Int, Int) -> Int -> (Map ((Int, Int), ((Int, Int), Int)) Int, Int)
    subc' mem' c xy 1 = memoise2 mem' f c (xy, 1)
      where
        f c (xy, 1) = minLength . S.toList $ sequencerC (M.elems dirMap) c xy
    subc' mem' c xy d = memoRec2 mem' f c (xy, d)
      where
        f m' c (xy, d) = S.foldl (\(m, r) s -> second (min r) $ sub m (d - 1) s) (m', maxBound) $ sequencerC (M.elems dirMap) c xy

    sub :: Map ((Int, Int), ((Int, Int), Int)) Int -> Int -> String -> (Map ((Int, Int), ((Int, Int), Int)) Int, Int)
    sub mem' d s = second snd $ foldl f (mem', (dirMap M.! 'A', 0)) s
      where
        f (m, (p, r)) e = second (\r' -> (dirMap M.! e, r + r')) $ subc' m (dirMap M.! e) p d

getComplexity :: Int -> String -> Int
getComplexity depth code = trace (show sequenceLength ++ "*" ++ show codeValue) codeValue * sequenceLength
  where
    codeValue = read $ takeWhile isDigit code
    numSequences = numericalSequencer code

    sequenceLength = snd $ foldl (\(m, r) s -> second (min r) $ depthMemorySequencer m depth s) (M.empty, maxBound) numSequences

part1 :: Input -> Output
part1 = sum . map (getComplexity 2)

part2 :: Input -> Output
part2 = sum . map (getComplexity 25)

main :: IO ()
main = do
  args <- getArgs
  content <- readFile (last args)
  let input = parseInput content

  print input

  print $ part1 input -- (== 248684) $
  print $ part2 input -- (== 307055584161760) $
