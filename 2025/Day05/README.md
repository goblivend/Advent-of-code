---
title: 2025/Day05
---

## Parsing:

Parsing was pretty straight forward:

1. split on the main delimiter : `\n\n`
2. handle the first part like Day02
3. Read each line into an int

```hs
type Input = ([(Int, Int)], [Int])

parseInput :: String -> Input
parseInput = (***) firstParse (map read) . both lines . toTuple . splitOn "\n\n"
  where
    toTuple (a : b : _) = (a, b)
    firstParse = map (toTuple . map read . splitOn "-")
```

## Part 1:

We need to find how many ingredients belong to the given ranges.

Nothing too spectacular about that, should use `count` instead of `length . filter`, but still not decided about using another lib.

```hs
part1 :: Input -> Output
part1 (fresh, ingredients) = length . filter (\i -> any (\(mi, ma) -> mi <= i && i <= ma) fresh) $ ingredients
```

## Part 2:

Here the exercise gets a bit trickier.

You need to count how many valid ids are possible given the ranges.

Except, the lazy approach of putting them in a set does not work, it's too big.

So I decided to merge the ranges than can be merged and to count the elements using the limits


```hs
part2 :: Input -> Output
part2 = sum . map ((+) 1 . uncurry (flip (-))) . mergeRanges . sortOn fst . fst -- summing the size of each merged range after having sorted them
  where
    mergeRanges [lh] = [lh] -- We only remove one element at a time so list never empty
    mergeRanges ((l1, h1) : (l2, h2) : l) -- if at least 2 elements, we might merge them
      | h1 < l2 = (l1, h1) : mergeRanges ((l2, h2) : l) -- l1 < l2 but if h1 < l2 then ranges separate
      | otherwise = mergeRanges ((l1, max h1 h2) : l) -- here ranges overlap and finding highest maximum bound
```

## Complete Code:

```hs
type Input = ([(Int, Int)], [Int])

parseInput :: String -> Input
parseInput = (***) firstParse (map read) . both lines . toTuple . splitOn "\n\n"
  where
    toTuple (a : b : _) = (a, b)
    firstParse = map (toTuple . map read . splitOn "-")

part1 :: Input -> Output
part1 (fresh, ingredients) = length . filter (\i -> any (\(mi, ma) -> mi <= i && i <= ma) fresh) $ ingredients

part2 :: Input -> Output
part2 = sum . map ((+) 1 . uncurry (flip (-))) . mergeRanges . sortOn fst . fst
  where
    mergeRanges [lh] = [lh]
    mergeRanges ((l1, h1) : (l2, h2) : l)
      | h1 < l2 = (l1, h1) : mergeRanges ((l2, h2) : l)
      | otherwise = mergeRanges ((l1, max h1 h2) : l)
```
