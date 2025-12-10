module Main where

import Control.Parallel.Strategies
import Data.Bits
import Data.List
import Data.List.Extra (maximumOn)
import Data.List.Split (splitOn)
import Data.List.Unique
import Data.Map (Map)
import Data.Map qualified as M
import Data.Matrix (Matrix, (!))
import Data.Matrix qualified as Mat
import Data.Maybe
import Data.Set (Set)
import Data.Set qualified as S
import Data.Tuple.Extra
import Debug.Trace
import System.Environment
import Text.Regex.TDFA ((=~))

-- TODO: Cleanup imports after day done

-- lights buttons jolt
type Input = [([Bool], [[Int]], [Int])]

type Output = Int

parseInput :: String -> Input
parseInput = map (getMachine . map (tail . init) . words) . lines
  where
    getMachine mch =
      ( map (== '#') $ head mch,
        map (read . ("[" ++) . (++ "]")) . tail . init $ mch,
        read . ("[" ++) . (++ "]") $ last mch
      )

-- TODO: Clean up part 1
part1 :: Input -> Output
part1 = sum . map pressesToturnOn
  where
    pressButtons :: Int -> Int -> [[Int]] -> [Int]
    pressButtons i maxi [] = replicate (maxi - i) 0
    pressButtons i maxi (b : bs)
      | i == head b = length b : pressButtons (i + 1) maxi bs
      | i < head b = 0 : pressButtons (i + 1) maxi (b : bs)
    pressesToturnOn :: ([Bool], [[Int]], [Int]) -> Int
    pressesToturnOn (tLights, buttons, _) = length . head $ filter ((== tLights) . map odd . pressButtons 0 nbLights . group . sort . concat) buttonSeqs
      where
        nbLights = length tLights
        buttonSeqs :: [[[Int]]]
        buttonSeqs = sortOn length . subsequences $ buttons

takeAll _ [] = []
takeAll f (e : l) = e : takeWhile ((== f e) . f) l

part2 :: Input -> Int
part2 = sum . parMap rseq (fromJust . (\(bs, jts) -> f (M.size bs) bs jts) . first (M.fromAscList . zip [0 ..]) . second (M.fromAscList . zip [0 ..])) . map (\(_, y, z) -> (y, z))
  where
    f :: Int -> Map Int [Int] -> Map Int Int -> Maybe Int
    f nbBs buttons tJolts
      | (any (< 0) tJolts) = Nothing -- error ("Negative Jolt should never happen: " ++ show tJolts) -- trace (show ("New Call With", nbBs, buttons, tJolts))
      | all (== 0) tJolts = Just 0 -- Found one requiring no more presses
      | M.null no0Buttons = Nothing
      | not allEasyDoable = Nothing -- Some Jolts Ids are undoable
      | length leastButtons == 1 = (+ pressFor1Bt) <$> f nbBs (M.delete onlyBt no0Buttons) (useButton onlyBt pressFor1Bt) --  If one jolt has only one possible button, we need to press it
      | otherwise = (listToMaybe . sort $ allPossibleRess) -- trace (show (biggestButton, pullRange))
      where
        no0Jolts :: [Int] -- All joltsIds with non 0 needs
        no0JoltsMp = M.filter (/= 0) tJolts
        no0Jolts = M.keys $ no0JoltsMp

        no0Buttons :: Map Int [Int] -- All buttons that can be used
        no0Buttons = M.filter (all (`elem` no0Jolts)) buttons

        biggestButton :: Int -- Index of button adding the most jolt
        biggestButton = maximumOn (length . (buttons M.!)) $ M.keys no0Buttons

        biggestPull :: Int -- The most times we can use that button
        biggestPull = minimum $ map (tJolts M.!) $ buttons M.! biggestButton
        pullRange = if M.size no0Buttons == 1 then [biggestPull] else [biggestPull, biggestPull - 1 .. 0] -- the range corresponding to the number of times to use biggest button
        useButton :: Int -> Int -> Map Int Int -- Adjust the current target Jolts by using Biggest Button n times
        useButton bId n = foldl (\js jId -> M.adjust ((+) (-n)) jId js) tJolts $ buttons M.! bId

        {- ---------- Checking all solution of biggest button ------- -}

        allRess :: [(Int, Maybe Int)] -- Map Int Int if collecting each button press
        allRess = map (\n -> (n, f nbBs (M.delete biggestButton no0Buttons) ({-(\e -> trace (show ("Removing", n, e, " From ", tJolts)) e) $  -} useButton biggestButton n))) pullRange

        allPossibleRess :: [Int]
        allPossibleRess = map ((uncurry (+)) . second fromJust) $ filter (isJust . snd) allRess

        {- ------------ Checking if a Jolt has only one matching Button ---------- -}

        (leastButtonJId, leastButtons) : _ = sortOn (length . snd) $ validButtons -- Jolt with the least number of buttons
        [onlyBt] = leastButtons
        pressFor1Bt = tJolts M.! leastButtonJId

        {- ---------- Checking if all values are solvable ------- -}

        -- Map from jid to associated button ids
        validButtons :: [(Int, [Int])] -- Jolts and their associated tuples
        validButtons = map (\j -> (\e -> (j, e)) . filter (\i -> j `elem` (buttons M.! i)) $ M.keys no0Buttons) no0Jolts
        validButtonsMp = M.fromAscList validButtons

        allEasyDoableForJt jt = (>= tJolts M.! jt) . sum . map (minimum . map ((M.!) tJolts) . (M.!) buttons) $ validButtonsMp M.! jt
        allEasyDoable = all allEasyDoableForJt no0Jolts

main :: IO ()
main = do
  args <- getArgs
  content <- readFile (last args)
  let input = parseInput content

  -- print input

  print $ part1 input
  print $ part2 input
