---
title: 2025/Day12
---

## Parsing:

So taking into account the actual input, the parsing was the most difficult part...

The input was split in 2 different parts, first a list of 3x3 shapes

```txt
0:
###
##.
##.

1:
###
##.
.##
```

For each shape we have its id then a 3x3 grid exposing which position does the shape use (`#` means occupied, while `.` means free)

Then we have a list of spots:

```txt
4x4: 0 0 0 0 2 0
12x5: 1 0 1 0 2 2
```

For each spot we have its dimension (4x4, 12x5...) then an ordered list representing the number of shapes to fill (first element means first shape...)

To represent the input, I chose a map of booleans grids (`Map Int [[Bool]]`) to have easy access to the present, and also have easy manipulation with grids and booleans.

Then for the spots, simply a list of tuples containing both the dimension and the list of presents to fit (`[((Int, Int), [Int])]`)

```hs
type Input = (Map Int [[Bool]], [((Int, Int), [Int])])

parseInput :: String -> Input
parseInput = second (map (parseSpot . words) . last) . first (M.fromAscList . map parsePresent . init) . dupe . splitOn [[]] . lines
  where
    parsePresent (id : shape) = (read $ init id, map (map (== '#')) shape)
    parseSpot (dims : l) = (both read . second (tail . init) . break (== 'x') $ dims, map read l)

```

## Part 1:

Here comes the theft:

You are asked to filter the spots with the ones that can fit the presents.

The thing is, a simple filter with a focus on the size of the area as well as the sum of the sizes of the presents is enough to get the right result...

What began as a gross filter to remove worst lines, ended up the final solution.

```hs
sizePresent :: [[Bool]] -> Int
sizePresent = length . filter id . concat

part1 :: Input -> Output
part1 (presents, spots) = length . filter canFit $ spots
  where
    canFit ((w, h), press) = (<= w * h) . sum . map (uncurry (*)) . zip press . map sizePresent $ M.elems presents
```

## Part 1, but real this time:

For the real version, I was planning on creating each possible combination and try and fit them to place, in theory it should have worked but my guess is it would have been even longer than day10...

```hs
flipPresent = map reverse

rotatePresent = reverse . transpose
```

## Complete Code:

```hs
type Input = (Map Int [[Bool]], [((Int, Int), [Int])])

parseInput :: String -> Input
parseInput = second (map (parseSpot . words) . last) . first (M.fromAscList . map parsePresent . init) . dupe . splitOn [[]] . lines
  where
    parsePresent (id : shape) = (read $ init id, map (map (== '#')) shape)
    parseSpot (dims : l) = (both read . second (tail . init) . break (== 'x') $ dims, map read l)

sizePresent :: [[Bool]] -> Int
sizePresent = length . filter id . concat

part1 :: Input -> Output
part1 (presents, spots) = length . filter canFit $ spots
  where
    canFit ((w, h), press) = (<= w * h) . sum . map (uncurry (*)) . zip press . map sizePresent $ M.elems presents
```
