---
title: 2025/Day11
---

## Parsing:

First graph of the year, but it has already been seen before, the best way to represent a graph under these conditions in haskell is through a map, from a key to the ones of its directs links

Here all the keys have 3 letters so we can optimize and split at index 3 to get the key and the list of destinations.

```hs
type Input = Map String [String]

parseInput :: String -> Input
parseInput = M.fromList . map (second (words . drop 1) . splitAt 3) . lines
```

## Part 1:

We are asked to get the number of path from `"you"` to `"out"`, since we are asked the number of paths and not the number of "direct" paths, we now that no cycles exist, otherwise we would have an infinity of paths.

So we can simplify a DFS for that:

1. If we are at destination return 1
2. otherwise, return the sum of paths from the linked keys to the exit

```hs
findAllPaths :: Map String [String] -> String -> String -> Map String Int
findAllPaths devs dest curr
  | dest == curr = 1
  | otherwise = sum ress
  where
    nextKeys = devs M.! curr
    ress = map (findAllPaths devs dest) nextKeys

part1 :: Input -> Output
part1 input = findAllPaths input "you" "out"
```

## Part 2:

Now for part 2, it gets trickier, we have to find a path from `"svr"` to `"out"` passing through both `"fft"` and `"dac"` in any order

One way to do it, the stupid but effective way is to sum the results of all the combinations of paths:

- "svr" "fft" "dac" "out"
- "svr" "dac" "fft" "out"

And for each of these 2 paths, multiply the results of each of the subpaths (eg: from `"svr"` to `"dac"`)

So a specific version would be

```hs
-- Header function to simplify results access from main functions part1 and part2
nbPaths :: Map String [String] -> String -> String -> Int
nbPaths input from to = findAllPaths input to from

part2 :: Input -> Output
part2 input = (svrDac * dacFft * fftOut) + (svrFft * fftDac * dacOut)
  where
    svrFft = nbPaths input "svr" "fft"
    svrDac = nbPaths input "svr" "dac"
    dacFft = nbPaths input "dac" "fft"
    fftDac = nbPaths input "fft" "dac"
    fftOut = nbPaths input "fft" "out"
    dacOut = nbPaths input "dac" "out"
```

A way to create a more generic version is to realize how the paths are created:

- take all permutations of must sees
- add the source and destination

This way you have all the paths, you just need to zip with a delta of one element to have the pairs of source and destination of each subpath.

```hs
passingThrough :: [String] -> Input -> String -> String -> Int
passingThrough mustSee input from to = sum . map (product . map (uncurry (nbPaths input)) . (uncurry zip) . second (drop 1) . dupe) $ ways
  where
    ways = map ((++ [to]) . (from :)) $ permutations mustSee

part2 :: Input -> Output
part2 input = passingThrough ["fft", "dac"] input "svr" "out"
```

Another optimization, useless in this case would be to store the results of each subpath since from 3 must sees we would have some multiple times.

We might think that the exercise is done and dusted.. but no.

The actual graph is wayyy to big to do the path finding using the same function as part1, it needs to be updated:

At any time, we need to remember how many paths we found from any node to the destination (or each current destination since with part2 we will be calling it 8 times).

The new algorithm goes as follows:

1. If current node as already been calculated previously, return
2. If we are at destination return a singleton informing that this key has 1 path
3. If the current node has no outputs, return a singleton informing that the key has 0 paths (for `"out"` when it is not the output)
4. otherwise:

    4.1. For the first next key, call back the function with the result map in parameters

    4.2. For all the remaining keys, call back the function with the map returned from the previous key and merge both maps together

    4.3. Return the final map after updating it with the sum of the results of the direct links of the current nodes (`nextKeys`)

5. The final result will just be the value associated to the key `from` in the map

```hs
findAllPaths :: Map String [String] -> Map String Int -> String -> String -> Map String Int
findAllPaths devs calculated dest curr
  | M.member curr calculated = calculated
  | dest == curr = M.singleton dest 1
  | M.notMember curr devs = M.singleton curr 0
  | otherwise = M.insert curr (sum $ map ((M.!) uniontMp) nextKeys) uniontMp
  where
    nextKeys = devs M.! curr
    uniontMp :: Map String Int
    uniontMp = foldl (\m k -> M.union m $ findAllPaths devs m dest k) calculated nextKeys

nbPaths :: Map String [String] -> String -> String -> Int
nbPaths input from to = (M.! from) $ findAllPaths input M.empty to from
```


## Complete Code:

```hs
type Input = Map String [String]

parseInput :: String -> Input
parseInput = M.fromList . map (second (words . drop 1) . splitAt 3) . lines

findAllPaths :: Map String [String] -> Map String Int -> String -> String -> Map String Int
findAllPaths devs calculated dest curr
  | M.member curr calculated = calculated
  | dest == curr = M.singleton dest 1
  | M.notMember curr devs = M.singleton curr 0
  | otherwise = M.insert curr (sum $ map ((M.!) uniontMp) nextKeys) uniontMp
  where
    nextKeys = devs M.! curr
    uniontMp :: Map String Int
    uniontMp = foldl (\m k -> M.union m $ findAllPaths devs m dest k) calculated nextKeys

nbPaths :: Map String [String] -> String -> String -> Int
nbPaths input from to = (M.! from) $ findAllPaths input M.empty to from

part1 :: Input -> Output
part1 input = nbPaths input "you" "out"

passingThrough :: [String] -> Input -> String -> String -> Int
passingThrough mustSee input from to = sum . map (product . map (uncurry (nbPaths input)) . (uncurry zip) . second (drop 1) . dupe) $ ways
  where
    ways = map ((++ [to]) . (from :)) $ permutations mustSee

part2 :: Input -> Output
part2 input = passingThrough ["fft", "dac"] input "svr" "out"
```
