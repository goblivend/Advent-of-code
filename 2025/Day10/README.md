---
title: 2025/Day10
---

## Parsing:

As usual, the parsing part went pretty smoothly.

It is composed of several machines, each machine represented as a list of ON/OFf states for lights (`.` for OFF and `#` for ON), a few lists of button changes and a list of values to finish the line

```txt
[.##.] (3) (1,3) (2) (2,3) (0,2) (0,1) {3,5,4,7}
```

So we basically need to parse 3 parts from a list of "words" (separated by spaces)

1. For the Lights we simply set a boolean to the presence of `#` on the first word
2. For the buttons, the arrays are almost formed, we just need to change the `()` into `[]` and we can just read the list from second to penultimate
3. For the Values, just like the buttons, reading is easy enough

```hs
--             Lights  Buttons  Values
type Input = [([Bool], [[Int]], [Int])]

parseInput :: String -> Input
parseInput = map (getMachine map (tail . init). words) . lines
  where
    getMachine mch = (
        map (== '#') $ head mch,
        map (read . ("[" ++) . (++ "]")) . tail . init $ mch,
        read . ("[" ++) . (++ "]") $ last mch
    )
```

## Part 1:

For part1, we understand that each button toggles the status of some lights, specified by the indices present in the list:

If I were to press the button `(2,3,5)`, I would pass from light status `......` to `..##.#`.

And that at the start, all lights are OFF, and our job is to find the least amount of presses to turn these lights ON.

Two tips we can now deduce from the subject are :

1. Pushing a button twice is equivalent to not pressing it at all.
2. The order of pushing buttons does not matter.

Knowing that, the list of all the possible button presses we can do is the list of subsequences of buttons.

Since we want the shortest number of presses, sorting the list of subsequences per number of button is the best way to stop at the shortest solution.

Now to find the result, my solution is quite crude and not very cleaned at the moment (TODO), but well...

What I do for each machine is

1. Find the sequence of presses
2. map to the toggles, (pressing the buttons `(0,3)`, and `(2,3)` would create the list of toggles `[0,3,2,3]`)
3. Sort the list of toggle to group each indices together (from the example above, we would get `[[0],[2],[3,3]]`)
4. Then set the actual lights:

    4.1. -- TODO Finish README


```hs
part1 :: Input -> Output
part1 = sum . map pressesToturnOn
  where
    pressButtons :: Int -> Int -> [[Int]] -> [Int]
    pressButtons i maxi [] = replicate (maxi - i) 0
    pressButtons i maxi (b : bs)
      | i == head b = length b : pressButtons (i + 1) maxi bs
      | i < head b = 0 : pressButtons (i + 1) maxi (b : bs)

    pressesToturnOn :: ([Bool], [[Int]], [Int]) -> Int
    pressesToturnOn (tLights, buttons, _) = length . head $ filter ((== tLights) . map odd . pressButtons 0 nbLights . group . sort . concat) buttonSeqs
      where
        nbLights = length tLights
        buttonSeqs :: [[[Int]]]
        buttonSeqs = sortOn length . subsequences $ buttons
```

## Part 2:

```hs
```

## Complete Code:

```hs

```
