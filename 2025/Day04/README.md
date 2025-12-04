# 2025/Day04:

## Parsing

Today there was the first interpretation of the input needed, I chose to map it as a Matrix of Booleans informing about the presence of a paper roll.

```hs
type Input = Matrix Bool

parseInput :: String -> Input
parseInput = Mat.fromLists . map (map (== '@')) . lines
```

## Part 1

For the main algorithm, I first select all the positions in the grid and filter them if they have a roll.

Then I need to check the number of neighbors containing roll :
- if only the 8 surroundings then strictly less than 4
- But since I also take my position which has a roll then I filter less than or equal to 4

To then remove the selected ones, I simply fold the list and remove each position one by one by changing the value to `False`



```hs
removeRols :: Matrix Bool -> Matrix Bool
removeRols input = foldl removeRol input . filter ((<= 4) . nbNeighbors) $ filter (input !) [(x, y) | x <- [1 .. w], y <- [1 .. h]]
  where
    w = Mat.ncols input
    h = Mat.nrows input
    removeRol = flip (Mat.setElem False) -- Changing Roll information to False
    nbNeighbors = length . filter (input !) . listNeighbors -- Getting the number of elements with rolls in my neighborhood
    listNeighbors (x, y) = filter checkBound [(x + dx, y + dy) | dx <- [-1 .. 1], dy <- [-1 .. 1]] -- Find neighbors in 3x3 grid (some filtered out on the edges)
    checkBound (x, y) = x >= 1 && y >= 1 && x <= w && y <= h -- Checking if neighbor coordinates fit in the Matrix
```

Once the rolls removed, I just need to check their numbers both before and after.

```hs
nbRolls :: Matrix Bool -> Int
nbRolls = length . filter id . Mat.toList -- Counting the number of elements set to True in the Matrix (Places with rolls)

part1 :: Input -> Output
part1 input = nbStart - nbEnd -- Taking the difference between the number of rolls at the start and at the end
  where
    nbStart = nbRolls input
    nbEnd = nbRolls $ removeRols input
```

## Part 2:

Part 2 is quite simple, I just use what I have done for part one and continue as long as rolls are removed

```hs
part2 :: Input -> Output
part2 input
  | nbEnd == nbStart = 0 -- Stop condition if rolls are not removed anymore
  | otherwise = nbStart - nbEnd + part2 matEnd -- Returning the current number rolls removed and the ones in the next step
  where
    nbStart = nbRolls input
    nbEnd = nbRolls matEnd
    matEnd = removeRols input
```


## Complete Code

```hs
type Input = Matrix Bool

parseInput :: String -> Input
parseInput = Mat.fromLists . map (map (== '@')) . lines

removeRols :: Matrix Bool -> Matrix Bool
removeRols input = foldl removeRol input . filter ((<= 4) . nbNeighbors) $ filter (input !) [(x, y) | x <- [1 .. w], y <- [1 .. h]]
  where
    (w, h) = (Mat.ncols input, Mat.nrows input)
    removeRol = flip (Mat.setElem False)
    nbNeighbors = length . filter (input !) . listNeighbors
    listNeighbors (x, y) = filter checkBound [(x + dx, y + dy) | dx <- [-1 .. 1], dy <- [-1 .. 1]]
    checkBound (x, y) = x >= 1 && y >= 1 && x <= w && y <= h

nbRolls :: Matrix Bool -> Int
nbRolls = length . filter id . Mat.toList

part1 :: Input -> Output
part1 input = nbStart - nbEnd
  where
    nbStart = nbRolls input
    nbEnd = nbRolls $ removeRols input

part2 :: Input -> Output
part2 input
  | nbEnd == nbStart = 0
  | otherwise = nbStart - nbEnd + part2 matEnd
  where
    nbStart = nbRolls input
    nbEnd = nbRolls matEnd
    matEnd = removeRols input
```
