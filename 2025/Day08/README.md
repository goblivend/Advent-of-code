---
title: 2025/Day08
---

## Parsing:

For today, the parsing went through 2 steps, first the initial parsing was quite simple:

Return a list of 3-tuples containing the boxes coordinates.

```hs
parseInput :: String -> [(Int, Int, Int)]
parseInput = map (read . (++ ")") . ("(" ++)) . lines
```

Then, when I got part2, I realised that actually, there was a lot of common calculation between the 2 parts, so I decided to add them here as well.

1. At first both part one and two ask to create a list of pairs of boxes sorted by their distance.
2. Then we are asked to link those pairs together into circuits for an unknown amount of pairs.

Knowing this, for the output I decided to have : `(nbBoxes, [(Pair, circuitsAfterPairLinked)])`

```hs
type Box = (Int, Int, Int)

type Input = (Int, [((Box, Box), [Set Box])])

parseInput :: String -> Input
parseInput = (&&&) length circuits . parse
  where
    -- Actual parsing of the input
    parse = map (read . (++ ")") . ("(" ++)) . lines
    -- Creating the list of pairs ordered by distance
    links = sortOn (uncurry distance) . pairs
    -- Creating the list of links and their respective circuit
    circuits = uncurry zip . second (drop 1 . scanl addLink []) . dupe . links

-- Euclidean distance squared to save a sqrt call on each pair
distance :: Box -> Box -> Int
distance (x1, y1, z1) (x2, y2, z2) = (x1 - x2) ^ 2 + (y1 - y2) ^ 2 + (z1 - z2) ^ 2

pairs :: [a] -> [(a, a)]
pairs l = [(x, y) | (x : l) <- tails l, y <- l]
```

This was the simple part, but 1 fuction is missing : `addLink`.

Here is how it works:

1. If no circuit, then create one with both boxes
2. If one of the boxes is in the circuit add the other
3. If none of the boxes are in the current circuit, try with the next one

But we have a catch:

if I have the pairs `[(a,b), (c, d), (b, c)]`

At the first call I will have:

circuit = `[]`, pair = `(a,b)`

which will add a new circuit `[[a, b]]`

At the second call, I will have:

circuit = `[[a,b]]`, pair = `(c,d)`

Since neither `c` nor `d` are in the first circuit, I will end up creating a new circuit : `[[a,b],[c,d]]`

And now, for the third call:

circuit = `[[a,b], [c,d]]`, pair = `(b,c)`

Here one of the element is in the first circuit but the other is in the second, so if I do nothing I will have :

circuit = `[[a,b,c],[c,d]]`

Which is incorrect since `c` is both in circuit 1 and 2, so I potentially need to merge the circuit following the insertion of a new box in a circuit.

This changes the step 2 to:

2. If one of the boxes is in the circuit add the other and merge this new circuit with the following ones

```hs
addLink :: [Set Box] -> (Box, Box) -> [Set Box]
addLink [] (xyz1, xyz2) = [S.fromList [xyz1, xyz2]]
addLink (s:l) (xyz1, xyz2)
  | S.member xyz1 s = merge l (S.insert xyz2 s)
  | S.member xyz2 s = merge l (S.insert xyz1 s)
  | otherwise = s : addLink l (xyz1, xyz2)
```

And now we need the function to actually merge the circuit:

1. If no circuit left, leave the circuit as is and add it to the list
2. If some elements are in the current circuit, then merge the two circuit add this new circuit to the list
3. If none of the element match, skip the current circuit and continue with the next ones

Since we only add 1 new element to the sets if we connect to an existing circuit, we don't need to try and merge the rest of the circuit once we find one to merge.

```hs
merge :: (Ord a) => [Set a] -> Set a -> [Set a]
merge [] s = [s]
merge (s' : l) s
  | not $ S.disjoint s' s = S.union s s': l
  | otherwise = s' : merge l s
```

## Part 1:

With this "parsing" finished, the first exercise is actually simple:

Multiply the sizes of the 3 biggest circuit after having done 1000 links.

```hs
part1 :: Input -> Output
part1 = product . take 3 . reverse . sort . map S.size . snd . head . drop 1000 . snd
```

## Part 2:

And the 2nd part is not more complicated:

Multiply the X coordinates of the 2 boxes creating the links that connects all boxes together

```hs
--             Multiply . take the X coordinate . of the first link for which the first circuit has the same size as the box set
part2 :: Input -> Output
part2 (n, l) = uncurry (*). both fst3 . fst . head . dropWhile (\(_, s) -> ( null  s) || S.size (head s) /= n) $ l
```

## Complete Code:

```hs
type Box = (Int, Int, Int)

type Input = (Int, [((Box, Box), [Set Box])])

type Output = Int

parseInput :: String -> Input
parseInput = (&&&) length circuits . parse
  where
    parse = map (read . (++ ")") . ("(" ++)) . lines
    links = sortOn (uncurry distance) . pairs
    circuits = uncurry zip . second (drop 1 . scanl addLink []) . dupe . links

distance :: Box -> Box -> Int
distance (x1, y1, z1) (x2, y2, z2) = (x1 - x2) ^ 2 + (y1 - y2) ^ 2 + (z1 - z2) ^ 2

pairs :: [a] -> [(a, a)]
pairs l = [(x, y) | (x : l) <- tails l, y <- l]

merge :: (Ord a) => [Set a] -> Set a -> [Set a]
merge [] s = [s]
merge (s' : l) s
  | not $ S.disjoint s' s = S.union s s': l
  | otherwise = s' : merge l s

addLink :: [Set Box] -> (Box, Box) ->  [Set Box]
addLink [] (xyz1, xyz2) = [S.fromList [xyz1, xyz2]]
addLink (s:l) (xyz1, xyz2)
  | S.member xyz1 s = merge l (S.insert xyz2 s)
  | S.member xyz2 s = merge l (S.insert xyz1 s)
  | otherwise = s : addLink l (xyz1, xyz2)

part1 :: Input -> Output
part1 = product . take 3 . reverse . sort . map S.size . snd . head . drop 1000 . snd

part2 :: Input -> Output
part2 (n, l) = uncurry (*) . both fst3 . fst . head . dropWhile (\(_, s) -> ( null  s) || S.size (head s) /= n) $ l
```
