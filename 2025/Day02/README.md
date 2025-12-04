# 2025/Day02

## Parsing

```txt
1-4,9-13
```

The input is represented as list of minimum and maximum bounds separated by a `-`, each pair separated by a `,`.

So I decided to represent them as a list of tuples.

To pare that list I transformed the input string and used the reading capabilities.

1. First I separate each pair into its own string by splitting the input on each `,`
2. Then I replace the `-` by `,` which are used in the tuple representation
3. I add the parenthesis to complete the tuple representation

```hs
type Input = [(Int, Int)]

parseInput :: String -> Input
parseInput = map (read . ("(" ++) . (++ ")") . replace "-" ",") . splitOn ","
```

## Part 1

For part one, we need to figure out which number has been created by copying the same pattern twice.

eg: `123123` can be created by copying the pattern `123` twice

So my solution I developed the following algorithm:

for the string

`abcabd`

`123456` -- *the indices*

1. I will first check that the number of digit is even:

    Here `6` is even

2. Then I will take the first half of the string:

    Here `abc` is my pattern

3. Next I will split the string on that pattern:

    My word starts with the pattern so the first result will be "".

    Then the word starts like the pattern but does not finish, the second result will be "abd"

    So the result will be `["", "abd"]`

4. To finish I will verify that all the strings are actually empty:

    In this example, the first element is empty but the second one is not, so the number can't be built by repeating the same pattern twice.


Here is the actual code checking this information

```hs
isInvalidForN :: Int -> String -> Bool
isInvalidForN n s
  | l `mod` n /= 0 = False -- the length of the word is not a multiple of the number of parts, it cannot be built
  | otherwise = all null $ splitOn patt s -- Are all the splits of the pattern empty
  where
    l = length s
    delta = div l n -- Get the length of the pattern
    patt = take delta s -- Get the pattern by taking the first chars in the string

sumInvalid :: (String -> Bool) -> [(Int, Int)] -> Int
sumInvalid isInvalid = sum . filter (isInvalid . show) . concat . map (uncurry enumFromTo) -- sum the invalid numbers within the given ranges

part1 :: Input -> Output
part1 = sumInvalid (isInvalidForN 2) -- get the results by checking with the pattern repeated twice
```

## Part 2

For part two, we need to check if there is a pattern repeated at least twice to form the number.

For this we can have the following algorithm:

1. Create a list of every number until the size of the string starting with 2 (all the number of times we can see the pattern repeating)

2. For every number, filter the ones for which the length of the string is a multiple

3. Using the previous algorithm, check if they are valid for each of them (stop at the first repeating pattern)

Now the code for this algorithm :

```hs
part2 :: Input -> Output
part2 = sumInvalid isInvalidForAll -- Sum all the invalid defined by the following function
  where
    isInvalidForAll s = not . null . filter (`isInvalidForN` s) $ [2 .. length s] -- Check if for any number from 2 to the length of the string you can find a repeating pattern
```


## Complete code

```hs
type Input = [(Int, Int)]

parseInput :: String -> Input
parseInput = map (read . ("(" ++) . (++ ")") . replace "-" ",") . splitOn ","

isInvalidForN :: Int -> String -> Bool
isInvalidForN n s
  | l `mod` n /= 0 = False
  | otherwise = all null . splitOn (take delta s) $ drop delta s -- Removing the first pattern to speed up the process
  where
    l = length s
    delta = div l n

sumInvalid :: (String -> Bool) -> [(Int, Int)] -> Int
sumInvalid isInvalid = sum . filter (isInvalid . show) . concat . map (uncurry enumFromTo)

part1 :: Input -> Output
part1 = sumInvalid isDouble
  where
    -- Here another version specific for 2 verifying if both sides are equal
    -- Slightly faster
    -- isDouble s = uncurry (==) $ splitAt (length s `div` 2) s
    -- Using existing code
    isDouble = isInvalidForN 2

part2 :: Input -> Output
part2 = sumInvalid isInvalidForAll
  where
    isInvalidForAll s = not . null . filter (`isInvalidForN` s) $ [2 .. length s]
```


An other version cousd have changed the code :
```hs
splitOn (take delta s) $ drop delta s
```

Into

```hs
uncurry splitOn $ splitAt delta s
```

to make it cleaner, but this solution is slower so I chose the one slightly less readable for optimization and to avoid confusion by thinking it does a half split
