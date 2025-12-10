module Main where

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
parseInput = map (getMachine . splitOn " ") . lines
  where
    getMachine mch =
      ( map (== '#') . init . tail $ head mch,
        map (read . ("[" ++) . (++ "]") . tail . init) . tail . init $ mch,
        read . ("[" ++) . (++ "]") . tail . init $ last mch
      )

pressButtons :: Int -> Int -> [[Int]] -> [Int]
pressButtons i maxi [] = replicate (maxi - i) 0
pressButtons i maxi (b : bs)
  | i == head b = length b : pressButtons (i + 1) maxi bs
  | i < head b = 0 : pressButtons (i + 1) maxi (b : bs)

part1 :: Input -> Output
part1 = sum . map pressesToturnOn
  where
    pressesToturnOn :: ([Bool], [[Int]], [Int]) -> Int
    pressesToturnOn (tLights, buttons, _) = length . head $ filter ((== tLights) . map odd . pressButtons 0 nbLights . group . sort . concat) buttonSeqs
      where
        nbLights = length tLights
        buttonSeqs :: [[[Int]]]
        buttonSeqs = sortOn length . subsequences $ buttons

takeAll _ [] = []
takeAll f (e : l) = e : takeWhile ((== f e) . f) l

part2 :: Input -> Int
part2 = sum . map snd . map (\e -> trace (show e) e) . zip [1 ..] . map fromJust . map ((\(bs, jts) -> f (M.size bs) bs jts) . first (M.fromAscList . zip [0 ..]) . second (M.fromAscList . zip [0 ..])) . map (\(_, y, z) -> (y, z))
  where
    f :: Int -> Map Int [Int] -> Map Int Int -> Maybe (Int)
    f nbBs buttons tJolts
      | (any (< 0) tJolts) = Nothing -- error ("Negative Jolt should never happen: " ++ show tJolts) -- trace (show ("New Call With", nbBs, buttons, tJolts))
      | all (== 0) tJolts = Just 0 -- (M.fromAscList . zip [0 .. nbBs - 1] $ repeat 0) -- Found one requiring no more presses
      | M.null no0Buttons = Nothing
      | length leastButtons == 1 = (+ pressFor1Bt) <$> f nbBs (M.delete onlyBt buttons) (useButton onlyBt pressFor1Bt) --  If one jolt has only one possible button, we need to press it
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

        allRess :: [(Int, Maybe (Int))] -- Map Int Int if collecting each button press
        allRess = map (\n -> (n, f nbBs (M.delete biggestButton buttons) ({-(\e -> trace (show ("Removing", n, e, " From ", tJolts)) e) $  -} useButton biggestButton n))) pullRange

        allPossibleRess = map (uncurry (+) . second fromJust) $ filter (isJust . snd) allRess

        validButtons :: [(Int, [Int])] -- Jolts and their associated tuples
        validButtons = map (\j -> (\e -> (j, e)) . filter (\i -> j `elem` (buttons M.! i)) $ M.keys buttons) no0Jolts
        (leastButtonJId, leastButtons) : _ = sortOn (length . snd) $ validButtons -- Jolt with the least number of buttons
        [onlyBt] = leastButtons
        pressFor1Bt = tJolts M.! leastButtonJId

main :: IO ()
main = do
  args <- getArgs
  content <- readFile (last args)
  let input = parseInput content

  -- print input

  print $ part1 input
  print $ part2 input

{-
Finding from Jolt element with least amount of buttons available
f nbBs buttons tJolts
  | any (< 0) tJolts = Nothing
  | all (== 0) tJolts = Just (M.fromAscList . zip [0 .. nbBs - 1] $ repeat 0) -- Found one requiring no more presses
  | otherwise = Nothing
  where
    no0Jolts :: [Int] -- All joltsIds with non 0 needs
    no0Jolts = M.keys $ M.filter (/= 0) tJolts
    validButtons :: [(Int, [Int])] -- Jolts and their associated tuples
    validButtons = map (\j -> (\e -> (j, e)) . filter (\i -> j `elem` (buttons M.! i)) $ M.keys buttons) no0Jolts
    (jId, bs) = head . sortOn (length . snd) $ validButtons -- Jolt with the least number of buttons
    targetJolts = tJolts !! jId

-}

{-

nbTapsToIncrs :: [[Int]] -> [Int] -> [[Int]]
nbTapsToIncrs buttons = foldl (\acc (v, i) -> acc ++ replicate i v) [] . zip buttons

      sum -- :: [Int] -> Int
        . head -- :: [[Int]]-> [Int]
        . filter
          ( (== tJoltage) -- :: [Int] -> Bool
              . pressButtons 0 nbJoltages -- :: [[Int]] -> [Int]
              . group
              . sort -- [Int] -> [[Int]] --Opti: groupOnKey id
              . concat -- [[Int]] -> [Int]
              . (nbTapsToIncrs no0buttons) -- :: [Int] -> [[Int]]
          ) -- :: [[Int]] -> [[Int]]
        $ buttonSeqs -- :: [[Int]] -- nb presses per buttons
      where
        nbJoltages = length tJoltage
        tJoltage0 = findIndices (== 0) tJoltage
        no0buttons = filter (not . any (`elem` tJoltage0)) buttons
        nbButtons = length no0buttons

        sol = [1, 3, 0, 3, 1, 2] -- [[3], [1, 3], [1, 3], [1, 3], [2, 3], [2, 3], [2, 3], [0, 2], [0, 1], [0, 1]]
        getSols l i = take i l ++ [l !! i + 1] ++ drop (i + 1) l

        buttonSeqs :: [[Int]]
        buttonSeqs = take 40 . concat $ iterate (concatMap (\sq -> map (getSols sq) [0 .. nbButtons - 1])) $ [replicate nbButtons 0]

-}
