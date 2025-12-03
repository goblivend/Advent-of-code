# Advent of code 2024

This year will do it in Haskell, but might try few days in Rust as well

- [x] Day 1
- [x] Day 2
- [x] Day 3
- [ ] Day 4
- [ ] Day 5
- [ ] Day 6
- [ ] Day 7
- [ ] Day 8
- [ ] Day 9
- [ ] Day 10
- [ ] Day 11
- [ ] Day 12

---

## Feedback

### Day 1:

Day one was nice.
A bit rusty, on haskell but ok enough, after solving it I thought of a way to use scanl instead of recursion

My first solution:
```hs
nextPos :: [Int] -> Int -> Output
nextPos [] pos = 0
nextPos (n : inp) pos = part1 inp $ mod (n + pos) 100

part1 :: Input -> Int -> Output
part1 input 0 = 1 + (nextPos input 0)
part1 input pos = (nextPos input pos)
```

and the new one using scanl

```hs
infixr 8 .:

(.:) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(.:) f g = \x y -> f (g x y)

turnDial :: Int -> Int -> Int
turnDial = ((`mod` 100) .: (+))

part1 :: Int -> Input -> Output
part1 = length . filter (== 0) .: scanl turnDial
```
I had to create a new operator in order to have function composition with not only 1 but 2 arguments.
This operator works just as `(.)`.

And same goes for part2 :

```hs
subdivide :: Int -> [Int]
subdivide n
  | abs n <= 100 = [n]
  | n < -100 = -100 : subdivide (n + 100)
  | n > 100 = 100 : subdivide (n - 100)

has0 :: Int -> Int -> Int -> Int
has0 prev next n
  | next == 0 = 1
  | abs n == 100 = 1
  | prev < next && n < 0 = 1
  | prev > next && n > 0 = 1
  | otherwise = 0

part2 :: Int -> Input -> Output
part2 _ [] = 0
part2 pos (v : l) = pass + part2 newpos (inp ++ l)
  where
    (n : inp) = subdivide v
    newpos = mod (n + pos) 100

    pass
      | pos == 0 && n < 0 = has0 100 newpos n
      | otherwise = has0 pos newpos n
```
For this solution I decided to split every number above 100 to simplify the process a little bit => `subdivide`
And as for the check part I needed to know whether we had passed through 0 or not
So i decided to write that function the most literally possible, then I simplified it

```hs
nbs0 :: Int -> Int -> Int
nbs0 prev n
  | n == 0 = 0
  | abs n >= 100 = cents n + nbs0 prev (signum n * rests n)
  | next == 0 = 1
  | signum n /= signum (next - prev) = 1
  | otherwise = 0
  where
    cents = (`div` 100) . abs
    rests = (`mod` 100) . abs
    next = turnDial prev n
```

same for the rest of part2, it got simplified

```hs
part2 :: Int -> Input -> Output
part2 = sum . map (uncurry nbs0 . fix0) . uncurry zip .: (&&& id) . scanl turnDial
  where
    fix0 (0, n) | n < 0 = (100, n)
    fix0 p = p
```

Quite happy of the simplification I managed to do, next step would be to try and create those the first time and not using a temporary version


### Day 2:

Today first solution was quite intuitive, then for the second one I tried different ideas to try and have a clean solution such as using Regex but didn't manage to find a clean solution.

So I did it by hand:

```hs
isSplitable :: String -> Bool
isSplitable s = not . null . filter isSame $ filter ((== 0) . (l `mod`)) [2 .. l]
  where
    l = length s
    isSame n = and $ allEqual <$> [(s !!) <$> [y * delta + x | y <- [0 .. n - 1]] | x <- [0 .. delta - 1]]
      where
        allEqual :: [Char] -> Bool
        allEqual (x : lst) = all (== x) lst

        delta = div l n

sumInvalid :: (String -> Bool) -> [(Int, Int)] -> Int
sumInvalid isInvalid = sum . filter (isInvalid . show) . concat . map (uncurry enumFromTo)

part1 :: Input -> Output
part1 = sumInvalid isDouble
  where
     isDouble s = uncurry (==) $ splitAt (length s `div` 2) s


part2 :: Input -> Output
part2 = sumInvalid isSplitable
```

for every number, I try to find factors of the number of digits then I check each letter (using lazy evaluation, so until it fails):

For the string

`abcabcabd`
`123456789`

I will try with 2 and see that 2 is not a factor of 9

so I'll try with 3 which is a factor

Next I'll compare all the first letters of the 3 splits : `a` `a` and `a` then to the `b`... up to the last `c` which is in fact a `d`

In the end I found a way too clean up `isSame`

```hs
isSame n = all null . splitOn (take delta s) $ drop delta s
```

which ended up the generic version of the 2 specific one, even if it is slightly slower

```hs
isdouble s = uncurry (==) $ splitAt (length s `div` 2) s
```

and the complete generic checker :

```hs
isInvalidForN :: Int -> String -> Bool
isInvalidForN n s
  | l `mod` n /= 0 = False
  | otherwise = all null . splitOn (take delta s) $ drop delta s
  where
    l = length s
    delta = div l n
```


For the final code

```hs
isInvalidForN :: Int -> String -> Bool
isInvalidForN n s
  | l `mod` n /= 0 = False
  | otherwise = all null . splitOn (take delta s) $ drop delta s
  where
    l = length s
    delta = div l n

sumInvalid :: (String -> Bool) -> [(Int, Int)] -> Int
sumInvalid isInvalid = sum . filter (isInvalid . show) . concat . map (uncurry enumFromTo)

part1 :: Input -> Output
part1 = sumInvalid (isInvalidForN 2)

part2 :: Input -> Output
part2 = sumInvalid isInvalidForAll
  where
    isInvalidForAll s = not . null . filter (`isInvalidForN` s) $ [2 .. length s]
```


### Day 3:

Today was interesting. I had to remember a few things about number:

1. The more digits, the higher the number
2. The higher digit at the beginning, the higher the number

That's one good way to feel stupid while coding...

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

so we can take any and we end up with `9`

for the complete result of `719`

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
