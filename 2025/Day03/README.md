---
title: 2025/Day03
---

## Algorithm

For todays puzzle you have to remember a few things about number:

1. The more digits, the higher the number
2. The higher digit at the beginning, the higher the number


That was the only trick to figure out today's puzzle:

- If you just have enough numbers to fill the 12 spots, just fill it
- If you have more take the first highest one in the spares (lets say we have 10 number and need 3, the highest one of the first 8 will be taken)

So an example could be:

`123457119` : for 3

For the first digit we can use the numbers

`1234571`

so we take `7`

then we simply have

`119`: for 2

we still need 1 number after so we can use `11`

the highest is `1`

we are left with

`19`: for 1

We can take any of them and choose the highest one `9`

The complete result is `719`

The resulting code in haskell :

```hs
bests :: Int -> [Int] -> [Int]
bests 0 l = [] -- If we don't need any more number that's it
bests i l
  | nbl <= i = l -- If we need all numbers, we take them
  | otherwise = best : bests (i - 1) (tail $ dropWhile (/= best) l) -- Otherwise, take the best from the available ones and continue with the rest of the list
  where
    nbl = length l
    best = maximum $ take (nbl - i + 1) l -- Taking the maximum number of digits available, find the highest one

findBestsOf :: Int -> [Int] -> Int
findBestsOf = read . concatMap show .: bests -- take the best combination of the line, then simple trick with shows and reads to assemble the numbers

part1 :: Input -> Output
part1 = sum . map (findBestsOf 2) -- Sum the results for each line with 2 batteries

part2 :: Input -> Output
part2 = sum . map (findBestsOf 12) -- Sum the results for each line with 12 batteries
```
