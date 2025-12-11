---
title: 2025/Day10
---

## Parsing:

As usual, the parsing part went pretty smoothly.

It is composed of several machines, each machine represented as a list of ON/OFf states for lights (`.` for OFF and `#` for ON), a few lists of button changes and a list of values to finish the line

```txt
[.##.] (3) (1,3) (2) (2,3) (0,2) (0,1) {3,5,4,7}
```

So we basically need to parse 3 parts from a list of "words" (separated by spaces)

1. For the Lights we simply set a boolean to the presence of `#` on the first word
2. For the buttons, the arrays are almost formed, we just need to change the `()` into `[]` and we can just read the list from second to penultimate
3. For the Values, just like the buttons, reading is easy enough

```hs
--             Lights  Buttons  Values
type Input = [([Bool], [[Int]], [Int])]

parseInput :: String -> Input
parseInput = map (getMachine map (tail . init). words) . lines
  where
    getMachine mch = (
        map (== '#') $ head mch,
        map (read . ("[" ++) . (++ "]")) . tail . init $ mch,
        read . ("[" ++) . (++ "]") $ last mch
    )
```

## Part 1:

For part1, we understand that each button toggles the status of some lights, specified by the indices present in the list:

If I were to press the button `(2,3,5)`, I would pass from light status `......` to `..##.#`.

And that at the start, all lights are OFF, and our job is to find the least amount of presses to turn these lights ON.

Two tips we can now deduce from the subject are :

1. Pushing a button twice is equivalent to not pressing it at all.
2. The order of pushing buttons does not matter.

Knowing that, the list of all the possible button presses we can do is the list of subsequences of buttons.

Since we want the shortest number of presses, sorting the list of subsequences per number of button is the best way to stop at the shortest solution.

Now to find the result, my solution is quite crude and not very cleaned at the moment (TODO), but well...

What I do for each machine is

1. Find the sequence of presses
2. Map to the toggles, (pressing the buttons `(0,3)`, and `(2,3)` would create the list of toggles `[0,3,2,3]`)
3. Sort the list of toggle to group each indices together (from the example above, we would get `[[0],[2],[3,3]]`)
4. Then set the actual lights:

    1. If no more light information is given, return a list of the remaining values to False
    2. If the current index of light matches the elements in the firs list of lights updated return if the number of elements in the sublist is odd (turned ON) and continue with the other elements
    3. If the current index is below the elements in the first sublist, it means the light was not trigger, return False and continue with the other elements of the list
5. Filter the sequences that match the target light statuses
6. Get the length of the first element (least number of buttons pressed to match the target)

```hs
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
```

## Part 2:

Now part 2 gets trickier, we have to do the same exercise, except that instead of light toggles, the buttons actually increase counters by 1 and we want to match the Values of the input.

To solve this problem, I decided to do it manually with several optimizations instead of looking for some magical math formula that would save me from hell.

First I ditch the lights and map the buttons and Values to Maps in order to have easy access in O(1) on each element.

Then I call my main function which either returns the number of button presses if possible or `Nothing` if it is impossible:

1. If all Values are set to 0, return that the problem can be solved with 0 button presses
2. No buttons can increase non-zero counters
3. Otherwise : Take the smallest value different from `Nothing` using the following algorithm
    1. Find all Value indices which need button presses (`no0Jolts`)
    2. Find the buttons that only increase those counters (`no0Buttons`)
    3. Find the button that increases the maximum number of counters (`biggestButton`)
    4. Find the maximum number of times this button can be pressed (`biggestPull`):
        1. Find the minimum target Value taking the indices of the `biggestButton`'s targets
    5. Create a range of number of times we can use that button (`pullRange`) sorted in reverse order so that we remove as much values as possible
        1. If there is only 1 button to be pressed, leave only `biggestPull` as value
        2. If there are more, create a decrementing range from `biggestPull` down to `0`
    6. For each of these possible values, press the button and call back the function:
        1. With N the current number of presses remove N from the Values of the target counters
        2. Remove the `biggestButton` from the Map of buttons
        3. Call back the main function
        4. If the call was successful, add N to the result
    7. Filter all the possible results and return the minimum one

```hs
part2 :: Input -> Int
part2 = sum . parMap rseq (fromJust . uncurry f . first mapIt . second mapIt . (\(_, y, z) -> (y, z)))
  where
    mapIt = M.fromAscList . zip [0 ..]
    f :: Map Int [Int] -> Map Int Int -> Maybe Int
    f buttons tJolts
      | all (== 0) tJolts = Just 0 -- Found one requiring no more presses
      | M.null no0Buttons = Nothing -- No button can increase counters
      | otherwise = (listToMaybe . sort $ allPossibleRess)
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
        allRess = map (\n -> (n, f (M.delete biggestButton no0Buttons) (useButton biggestButton n))) pullRange

        allPossibleRess :: [Int]
        allPossibleRess = map ((uncurry (+)) . second fromJust) $ filter (isJust . snd) allRess
```

A first update we can make is to take an easy guess as to whether the problem can be solved:
1. For each Value, find the buttons that can be pressed to increase that counter
2. From this button list find the maximum number of times we can press each button
3. Verify that the sum of those values is greater than the target Value

As said previously, here, if for some counter `V`, both buttons were stuck by a second value `V'`, we would check that `V <= V'*2` while the actual would be `V <= V'`

But since this leads to edge cases and a more heavy computation that may not be worth it, I decided that for the moment it would be enough.

```hs
part2 :: Input -> Int
part2 = sum . parMap rseq (fromJust . uncurry f . first mapIt . second mapIt . (\(_, y, z) -> (y, z)))
  where
    mapIt = M.fromAscList . zip [0 ..]
    f :: Map Int [Int] -> Map Int Int -> Maybe Int
    f buttons tJolts
      | all (== 0) tJolts = Just 0 -- Found one requiring no more presses
      | M.null no0Buttons = Nothing -- No button can be pressed
      | not allEasyDoable = Nothing -- Some Jolts Ids are undoable
      | otherwise = {- code seen previously -}
      where
        {- code seen previously -}

        {- ---------- Checking if all values are solvable ------- -}

        -- Map from jid to associated button ids
        validButtons :: [(Int, [Int])] -- Jolts and their associated tuples
        validButtons = map (\j -> (\e -> (j, e)) . filter (\i -> j `elem` (buttons M.! i)) $ M.keys no0Buttons) no0Jolts
        validButtonsMp = M.fromAscList validButtons

        allEasyDoableForJt jt = (>= tJolts M.! jt) . sum . map (minimum . map ((M.!) tJolts) . (M.!) buttons) $ validButtonsMp M.! jt
        allEasyDoable = all allEasyDoableForJt no0Jolts

```

Another optimization, is to check if for any Value, there is only 1 matching button that can be used:

1. Find the Value with the minimum number of buttons `(leastButtonJId, leastButtons)`
2. If this number of buttons `length leastButtons` is different from 1 break from this optimization, otherwise continue
3. Use this button as many times as required `pressFor1Bt` by the selected value `leastButtonJId`
4. Remove the button from the list of button
5. call back the function
6. In case of success, add the number of presses `pressFor1Bt` to the result

Since we decided to go greedy and use the button perhaps too many times, we also need to check whether any Value is negative in one of the first checks

```hs
part2 :: Input -> Int
part2 = sum . parMap rseq (fromJust . uncurry f . first mapIt . second mapIt . (\(_, y, z) -> (y, z)))
  where
    mapIt = M.fromAscList . zip [0 ..]
    f :: Map Int [Int] -> Map Int Int -> Maybe Int
    f buttons tJolts
      | (any (< 0) tJolts) = Nothing -- All values are correct
      | all (== 0) tJolts = Just 0 -- Found one requiring no more presses
      | M.null no0Buttons = Nothing -- No buttons can be pressed
      | not allEasyDoable = Nothing -- Some Jolts Ids are undoable
      | length leastButtons == 1 = (+ pressFor1Bt) <$> f (M.delete onlyBt no0Buttons) (useButton onlyBt pressFor1Bt) --  If one jolt has only one possible button, we need to press it
      | otherwise ={- code seen previously -}
      where
        {- code seen previously -}
        {- ------------ Checking if a Jolt has only one matching Button ---------- -}

        (leastButtonJId, leastButtons) = minimumOn (length . snd) $ validButtons -- Jolt with the least number of buttons
        [onlyBt] = leastButtons
        pressFor1Bt = tJolts M.! leastButtonJId
```

With all these optimizations, we land on the following code

```hs
part2 :: Input -> Int
part2 = sum . parMap rseq (fromJust . uncurry f . first mapIt . second mapIt . (\(_, y, z) -> (y, z)))
  where
    mapIt = M.fromAscList . zip [0 ..]
    f :: Map Int [Int] -> Map Int Int -> Maybe Int
    f buttons tJolts
      | (any (< 0) tJolts) = Nothing -- Was too greedy, the input is not coherent
      | all (== 0) tJolts = Just 0 -- Found one requiring no more presses
      | M.null no0Buttons = Nothing -- No button can be pressed
      | not allEasyDoable = Nothing -- Some Jolts Ids are undoable
      | length leastButtons == 1 = (+ pressFor1Bt) <$> f (M.delete onlyBt no0Buttons) (useButton onlyBt pressFor1Bt) --  If one jolt has only one possible button, we need to press it
      | otherwise = (listToMaybe . sort $ allPossibleRess)
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

        {- ------------ Checking if a Jolt has only one matching Button ---------- -}

        (leastButtonJId, leastButtons) = minimumOn (length . snd) $ validButtons -- Jolt with the least number of buttons
        [onlyBt] = leastButtons
        pressFor1Bt = tJolts M.! leastButtonJId

        {- ---------- Checking if all values are solvable ------- -}

        -- Map from jid to associated button ids
        validButtons :: [(Int, [Int])] -- Jolts and their associated tuples
        validButtons = map (\j -> (\e -> (j, e)) . filter (\i -> j `elem` (buttons M.! i)) $ M.keys no0Buttons) no0Jolts
        validButtonsMp = M.fromAscList validButtons

        allEasyDoableForJt jt = (>= tJolts M.! jt) . sum . map (minimum . map ((M.!) tJolts) . (M.!) buttons) $ validButtonsMp M.! jt
        allEasyDoable = all allEasyDoableForJt no0Jolts

```















## Complete Code:

```hs

```
