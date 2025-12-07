---
title: 2025/Day07
---

## Parsing:

Another day where parsing is important, today we have a grid.

In the grid there are 2 main things to save:

1. The start point `S`
2. The splitters `^`

The start point is always on the first line, so we can save its column and that will be enough

For the splitters, they are placed from the second line downward.

Even if that might not be very useful I decided to save the information as a grid, I will keep the useful lines (containing a splitter).

```hs
type Input = ([[Bool]], Int)

parseInput :: String -> Input
parseInput = (&&&) getMat getS . lines
  where
    getS = fromJust . elemIndex 'S' . head
    getMat = map (map (== '^')) . filterLines -- Creating a grid of booleans to inform on the presence of splitter
    filterLines = filter (elem '^') . tail -- Removing lines without splitters for optimizations
```

## Part 1:

For the first part, we need to know how many tachyons hit splitters

Since the tachyons always move downward, we only need to store their column.

The same goes for when they hit a splitter:

```hs
getNextCols :: [Bool] -> Int -> [Int]
getNextCols line x
  | line !! x = [x - 1, x + 1] -- Tachyon hit a splitter, now we have 2 tachyons
  | otherwise = [x] -- Tachyons continues to move downward until next splitter
```

Another useful thing about Tachyons moving downward is that we don't need to store the grid above, and we can ditch them along the way.

```hs
getNextCols :: [Bool] -> Int -> [Int]
getNextCols line x
  | line !! x = [x - 1, x + 1]
  | otherwise = [x]

part1 :: Input -> Output
part1 = uncurry help . second S.singleton
  where
    help :: [[Bool]] -> Set Int -> Int
    help [] xs = 0 -- End of the grid, no hit done
    help (line : m) xs = countHits + (help m newXs)
      where
        countHits = S.size $ S.filter (line !!) xs -- Number of tachyons present at the same column as a splitter == number of hits this turn
        newXs = S.unions $ S.map (S.fromAscList . getNextCols line) xs -- We map the tachyons position to their next, we keep a set to avoid duplicating data
```

## Part 2:

The part 2 is quite similar, but instead of counting hits, we need to count the number of different paths.

For this, brute force is absolutely useless, we need to do things smart.

Since we know that from a point downward, the number of paths is always the same, we can simply list the positions we are interested in and ask the number of possible paths of each column.

```hs
part2 :: Input -> Output
part2 = head . uncurry help . second (singleton)
  where
    help :: [[Bool]] -> [Int] -> [Int]
    help [] xs = 1 <$ xs -- Grid ended, no more splitter, only 1 path per column
    help (line : m) xs = map (sum . map (nextRes M.!) . (M.!) newXsmp) xs -- Returning the number of paths of each column (nb x or nb x-1 + nb x+1)
      where
        newXsmp :: Map Int [Int]
        newXsmp = M.fromAscList . zip xs . map (getNextCols line) $ xs -- Creating a map between the current tachyon columns, and the ones next iteration
        newXs = concat $ M.elems newXsmp -- Getting all the positions the tachyons will use at next iteration
        nextRes = M.fromAscList $ zip newXs (help m newXs) -- Mapping the position of next iteration tachyons to the number of path from there
```

## Complete Code:

```hs
type Input = ([[Bool]], Int)

parseInput :: String -> Input
parseInput = (&&&) getMat getS . lines
  where
    getS = fromJust . elemIndex 'S' . head
    getMat = map (map (== '^')) . filterLines
    filterLines = filter (elem '^') . tail

getNextCols :: [Bool] -> Int -> [Int]
getNextCols line x
  | line !! x = [x - 1, x + 1]
  | otherwise = [x]

part1 :: Input -> Output
part1 = uncurry help . second S.singleton
  where
    help :: [[Bool]] -> Set Int -> Int
    help [] xs = 0
    help (line : m) xs = countHits + (help m newXs)
      where
        countHits = S.size $ S.filter (line !!) xs
        newXs = S.unions $ S.map (S.fromAscList . getNextCols line) xs

part2 :: Input -> Output
part2 = head . uncurry help . second (singleton)
  where
    help :: [[Bool]] -> [Int] -> [Int]
    help [] xs = 1 <$ xs
    help (line : m) xs = map (sum . map (nextRes M.!) . (M.!) newXsmp) xs
      where
        newXsmp :: Map Int [Int]
        newXsmp = M.fromAscList . zip xs . map (getNextCols line) $ xs
        newXs = concat $ M.elems newXsmp
        nextRes = M.fromAscList $ zip newXs (help m newXs)
```
