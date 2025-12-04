# 2025/Day01

## Prerequisites

Today I realized I needed more than simply the given combinators `(.)` and `($)` which only allow one parameter added.

I needed some combinator to allow 2 parameters, that why I used `(.:)`

with this combinator I can have:

```hs
printAdded a b = print $ a+b
```
To be improved to

```hs
printAdded = print .: (+)
```

I decided to just take its definition, otherwise I would have to add another library just for that simple code.

```hs
infixr 8 .:

(.:) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(.:) f g = \x y -> f (g x y)
```

## Parsing

As given in the subject, lines with right move are positive numbers and left moves are negative numbers.

So my representation is a list of deltas to apply.

```hs
type Input = [Int]

parseInput :: String -> Input
parseInput = map mread . lines
  where
    mread ('L' : s) = -read s -- line starts with L : Negative
    mread ('R' : s) =  read s -- line starts with R : Positive
    mread _ = 0 -- Default case never reached
```

## Part 1

For part one, the solution is simple :

- Change the position by adding the current one with the new delta and keep inside the bounds with a modulo to 100
- Record every position
- Count the number of times you have 0

```hs
turnDial :: Int -> Int -> Int
turnDial = ((`mod` 100) .: (+)) -- Turning from the position given as one of the parameters with the delta given as the other parameter

part1 :: Int -> Input -> Output
part1 = length . filter (== 0) .: scanl turnDial -- count the number of 0 elements you got after turning all deltas
```

## Part 2

For part two it's a little bit more complicated as you have more cases :

- if I hit 0 (part1)
- the number of times I passed through 0

```hs
nbs0 :: Int -> Int -> Int -- Count the numbers of 0 seen at one step
nbs0 prev n
  | n == 0 = 0 -- Stop case if I don't turn (case only reached from next condition if a multiple of 100 was the initial parameter)
  | abs n >= 100 = cents n + nbs0 prev (signum n * rests n) -- If at least a complete turn, add the number of complete turn to the usual results
  | next == 0 = 1 -- If I hit 0
  | signum n /= signum (next - prev) = 1 -- If the sign of the delta and the sign of the `result-position` do not match, I passed through 0
  | otherwise = 0 -- I didn't pass through 0
  where
    cents = (`div` 100) . abs -- Number of complete turns
    rests = (`mod` 100) . abs -- Delta left after all complete turns but as absolute value,
    next = turnDial prev n -- position after turning


part2 :: Int -> Input -> Output
part2 = sum . map (uncurry nbs0 . fix0) . uncurry zip .: (&&& id) . scanl turnDial -- Adding the number of 0 seen through each individual step
  where
    fix0 (0, n) | n < 0 = (100, n) -- Small issue with nbs0, if delta negative and position to 0 (positive), this 0 is counted twice, so need to change it to 100 instead
    fix0 p = p
```
