# Advent of code 2024

This year will do it in Haskell, but might try few days in Rust as well

- [x] Day 1
- [ ] Day 2
- [ ] Day 3
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
