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
    pressButtons :: Int -> Int -> [[Int]] -> [Bool]
    pressButtons i maxi [] = replicate (maxi - i) False
    pressButtons i maxi (b : bs)
      | i == head b = odd (length b) : pressButtons (i + 1) maxi bs
      | i < head b = False : pressButtons (i + 1) maxi (b : bs)
    pressesToturnOn :: ([Bool], [[Int]], [Int]) -> Int
    pressesToturnOn (tLights, buttons, _) = length . head $ filter ((== tLights) . pressButtons 0 nbLights . group . sort . concat) buttonSeqs
      where
        nbLights = length tLights
        buttonSeqs :: [[[Int]]]
        buttonSeqs = sortOn length . subsequences $ buttons

part2 :: Input -> Int
part2 = sum . parMap rseq (fromJust . uncurry f . first mapIt . second mapIt . (\(_, y, z) -> (y, z)))
  where
    mapIt = M.fromAscList . zip [0 ..]
    f :: Map Int [Int] -> Map Int Int -> Maybe Int
    f buttons tJolts
      | {-# SCC anyNegativeCheck #-} (any (< 0) tJolts) = Nothing -- Was too greedy, the input is no coherent
      | {-# SCC allDoneCheck #-} all (== 0) tJolts = Just 0 -- Found one requiring no more presses
      | {-# SCC no0ButtonsCheck #-} M.null no0Buttons = Nothing -- No button can be pressed
      | {-# SCC notAllEasyDoableCheck #-} not allEasyDoable = Nothing -- Some Jolts Ids are undoable
      | {-# SCC onlyOneButtonCheck #-} length leastButtons == 1 = {-# SCC only1ButtonForAJoltageRes #-} (+ pressFor1Bt) <$> f (M.delete onlyBt no0Buttons) (useButton onlyBt pressFor1Bt) --  If one jolt has only one possible button, we need to press it
      | otherwise = {-# SCC mainRes #-} (listToMaybe . sort $ allPossibleRess)
      where
        no0Jolts :: [Int] -- All joltsIds with non 0 needs
        no0JoltsMp = M.filter (/= 0) tJolts
        no0Jolts = M.keys $ no0JoltsMp

        no0Buttons :: Map Int [Int] -- All buttons that can be used
        no0Buttons = {-# SCC no0ButtonsCalculation #-} M.filter (all (`elem` no0Jolts)) buttons

        biggestButton :: Int -- Index of button adding the most jolt
        biggestButton = {-# SCC biggestButtonCalculation #-} maximumOn (length . (buttons M.!)) $ M.keys no0Buttons

        biggestPull :: Int -- The most times we can use that button
        biggestPull = minimum $ map (tJolts M.!) $ buttons M.! biggestButton
        pullRange = if M.size no0Buttons == 1 then [biggestPull] else [biggestPull, biggestPull - 1 .. 0] -- the range corresponding to the number of times to use biggest button
        useButton :: Int -> Int -> Map Int Int -- Adjust the current target Jolts by using Biggest Button n times
        useButton bId n = {-# SCC useButtonCalculation #-} foldl (\js jId -> M.adjust ((+) (-n)) jId js) tJolts $ buttons M.! bId

        {- ---------- Checking all solution of biggest button ------- -}

        allRess :: [(Int, Maybe Int)] -- Map Int Int if collecting each button press
        allRess = {-# SCC allRessCalculation #-} map (\n -> (n, f (M.delete biggestButton no0Buttons) (useButton biggestButton n))) pullRange

        allPossibleRess :: [Int]
        allPossibleRess = {-# SCC allPossibleRessFilterCalculation #-} map ((uncurry (+)) . second fromJust) $ filter (isJust . snd) allRess

        {- ------------ Checking if a Jolt has only one matching Button ---------- -}

        (leastButtonJId, leastButtons) = {-# SCC leastButtonJoltCalculation #-} minimumOn (length . snd) $ validButtons -- Jolt with the least number of buttons
        [onlyBt] = leastButtons
        pressFor1Bt = tJolts M.! leastButtonJId

        {- ---------- Checking if all values are solvable ------- -}

        -- Map from jid to associated button ids
        validButtons :: [(Int, [Int])] -- Jolts and their associated tuples
        validButtons = {-# SCC validButtonsCalculation #-} map (\j -> (\e -> (j, e)) . filter (\i -> j `elem` (buttons M.! i)) $ M.keys no0Buttons) $ no0Jolts
        validButtonsMp = {-# SCC validButtonsMapCreation #-} M.fromAscList validButtons

        allEasyDoableForJt jt = {-# SCC allEasyDoableForJtCalculation #-} (>= tJolts M.! jt) . sum . map (minimum . map ((M.!) tJolts) . (M.!) buttons) $ validButtonsMp M.! jt
        allEasyDoable = {-# SCC allEasyDoableCalculation #-} all allEasyDoableForJt no0Jolts

main :: IO ()
main = do
  args <- getArgs
  content <- readFile (last args)
  let input = parseInput content

  -- print input

  print $ part1 input
  print $ part2 input
