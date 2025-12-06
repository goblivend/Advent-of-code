---
title: 2025/Day05
---

## Parsing:

For the input we have a list of operations to perform, except, the notation is different.

We have column of operations and one operator for the whole column.

```txt
12 21 3  1
 2  1 42 2
*  +  *  *
```

In order to have useful input, there is a need to separate the operands and the operators.

So we will sort the operations as a list of tuples containing the operator and the values.
```hs
type Input = [((Int -> Int -> Int), [String])]
```
We do not yet try to parse the values themselves since it will be different in part 1 and 2.
This is also the reason why we need to keep the spaces in the value's strings


Here is what we will do:

1. Split each line to have a matrix
2. Transpose the matrix to have the operators and operands on the same "line"
3. Split the matrix into a list of matrix which are separated by lines of spaces (to split by operations)
4. Transpose the sub matrixes to separate the operands and the operator
5. Parse the operator

```hs
type Input = [((Int -> Int -> Int), [String])]

parseInput :: String -> Input
parseInput = map getOp . map transpose . split (all (== ' ')) . transpose . lines
  where
    getOp = (***) (readOp . last) init . dupe
    readOp s
      | elem '*' s = (*)
      | elem '+' s = (+)
```

## Part 1:

With this input, I just need to sum of each operations.

To calculate the operations we can have our friend `foldl` do the work, more precisely `foldl1` to use the 2 first values instead of a default one on the list.

And to get the values, we just need to read the integer written in each line.

```hs
part1 :: Input -> Output
part1 = sum . map (uncurry foldl1 . second (map read))
```

## Part 2:

Now for part2, we need to do something first.

The values are not actually read like we think, they're actually written in column instead of lines:

```txt
123
 43
 5
```
is not the list `[123, 43, 5]`
but the list `[33, 245, 1]`

But since we kept the spaces, we can simply transpose the list of string and have the correct list of values.

```txt
1
245
33
```
Then we just need to do like part1 to get the results.

```hs
part2 :: Input -> Output
part2 = part1 . map (second transpose)
```

## Complete Code:

```hs
type Input = [((Int -> Int -> Int), [String])]

type Output = Int

parseInput :: String -> Input
parseInput = map getOp . map transpose . split (all (== ' ')) . transpose . lines
  where
    getOp = (***) (readOp . last) init . dupe
    readOp s
      | elem '*' s = (*)
      | elem '+' s = (+)

part1 :: Input -> Output
part1 = sum . map (uncurry foldl1 . second (map read))

part2 :: Input -> Output
part2 = part1 . map (second transpose)
```
