# Advent of code 2024

This year will do it in Haskell, but might try few days in Rust as well

- [x] Day 1
- [x] Day 2
- [x] Day 3
- [x] Day 4
- [x] Day 5
- [x] Day 6
- [x] Day 7
- [x] Day 8
- [x] Day 9
- [x] Day 10
- [x] Day 11
- [x] Day 12
- [x] Day 13
- [x] Day 14
- [x] Day 15
- [x] Day 16
- [?] Day 17
- [x] Day 18
- [ ] Day 19
- [ ] Day 20
- [ ] Day 21
- [ ] Day 22
- [ ] Day 23
- [ ] Day 24
- [ ] Day 25

---

## Feedback

### Day 1:

As every year, still a bit rusty but managed to do it anyways

### Day 2:

Still rusty even found the part2 a bit difficult (might have been trying to do something opti since the beginning but that complexified a lot....)

### Day 3:

Today seemed complicated at first with the parsing but actually instead of using Read instances as I planned at the beginning, using Regex was easier.
The part 2 didn't suprise me too much, I just thought there would have been more operations instead of just `do` `don't` and `mul`...

### Day 4:

Interesting puzzle, quite happy of my solution and the possible ways to debug it.

I cleaned it afterward since debugging is no more needed but was an interesting day

### Day 5:

First part nice

Second part subject does not seem complete enough
```
1|2
5|4
2|3
3|4

2,3,5,4,1
```
With this example a correct order could be
`1,2,3,5,4` or `1,2,5,3,4` or `1,5,2,3,4` or `5,1,2,3,4`
there is no way to decide. If it's the same thing in the inputs -> some valid solutions won't work

and worse there even are some circular dependencies in my input :
```
16|11
11|19
19|16
```
Not nice at all, if it's a page ordering => impossible to print some of the updates

(I understand that might be the goal of the exercise but then this lore can't work....)

So instead of creating a perfect list and choosing the elements inside, I need to create a valid list each time => way higher time complexity

### Day 6:

Quite happy about today managed to find a solution quite fast and minimize the amount of parsing

The only thing is that my code is quite ugly at the moment

For the part 2 I managed to speed up my solution :
- At first I tried adding a wall on every free spot using lists as memory => estimated 180mins
- Changing the memory list to a Set memory => 3mins
- Filtering the positions to only places where the guard passes in the their usual path => 30sec
- Adding -O2 => ~10sec
- Switching to Matrices instead of raw `[String]` => ~5.5s
- Multithreading => ~6s (runs slightly slower but at least I have a reference to use multithreading)

### Day 7:

Today was really easy
In order to optimize, I had to add profile costs centers check what operations took the most time

At first the bottle neck was the operation `concatNb`, I did it like `read (show a) (show b)` but I don't really need to read

So the solution was just to shift `a` to the left by multiplying it by `((^) 10) . length $ show b`. This does for example with `42` and `24` : `42* 10^2` => `4200` and then only `(+)` => `4224`

At the moment the bottle neck is the operation `(*)` so no really my fault.

### Day 8:

Todays was also fun.

Since solution already is at 0.03s, there is no need to optimize.

Instead I had fun creating a function to preview the actual grid at each step, you just have to uncomment the lines in main to see it.

### Day 9:

Today was nice, was not as easy but was fun to think about and find optimized solutions

My solution is not that fast today ~1m30s might try to optimize it further but at the moment need to work on other things
(ghc profiler decided not to profile my costs centers)

So I indeed took time to do some optimizations, now I run in 2s.

The main reason for this time improvement is because I now play using a tuple (id, size) and not just the id, hence less items to loop through

Tried more optimizations such as saving the first valid free spot but this was only slowing things down

### Day 10:

Today was so short that I didn't even take time to enjoy it...

It was like asking 2+2...

### Day 11:

First part was interesting, managed to realize that `++` is the worst operator ever and that you should always use `concat` instead

For the second part, I managed to optimize and go up to `38` quite far from the required `75`

Tried to multithread it (in the `iterateStones` change `map` to `parMap rseq`) but it only slowed things down...
(for target 35 had 4s in normal map and 12 using rseq...)

- rseq : 12
- r0 : 11.7
- rdeepseq: 9.6
- rpar : 12.7

Might try looking for loops and similar things

I finally got it, the solution I found was to use memoization

At first I had 2 maps :
- First map for the result of the algorithm given an element (not much gain)
- Second one for the number of stones after n blinks with the stone

Since the first map didn't help, I tried to clean up and realized it was really useless so I removed it completely

Had fun doing it in python as well, strangely python is faster than haskell

### Day 12:

That was a nice puzzle.

Might have been a bit easy but was interesting and difficult enough to have to think about it.

The only downside is what my solution looks like...

Cleaned that up a bit but still not great

### Day 13:

Not a huge fan of today

At least not in haskell

First I tried the greedy method of finding the nb of button presses by simulating every combinations.
Gave me answer for part1 but impossible for part2

Then I thought about solving `Diophantine equations` but seemed too much of a mess to find result, adjust them since I had to do them twice and find a potential balance

So the last solution was to use matrices, but then the Double precision was still not good.
I thought that to determine if a number was an `Int` I could compare by `1e-10` seemed low and steady.
But actually my solution works in haskell only for epsilon between [1e-2, 1e-3] so that does not leave much room for errors.
The worst thing is that since it's so small it was a guessing game to have the same result as another solution found on reddit, since at first I thought I had a mistake in my calculations but found nothing of the sort.....

### Day 14:

Today was quite simple.

I am not a huge fan of part2 today, telling us "Hey there will be a christmas tree" without telling us anything else ???
Like and example tree would have been nice or the size of the tree or anything about it...

### Day 15:

Today was interesting, it was not either too easy nor coming from nowhere

Even though my solution is very ugly, today I don't think I will clean it up.

### Day 16:

Nice and interesting puzzle today

Basically a simple path finding but with a few twists that make it fun

Will try to optimize solution

Edit: Haven't found any easy optimization

I know a way to optimize : Have another Map containing this time the set of valid spots => if already been on a position no need to do it again even from another good path.

For example
```
#######
#....E#
#.#.###
#S..###
#######
```

The first path explored will be this one
```
#######
#..OOE#
#.#O###
#SOO###
#######
```

But then at the moment when I reach the last intersection I need to go back up to the end

```
#######
#OOXXE#
#O#.###
#S..###
#######
```

The optimization would allow me to skip this last part represented with `X`

### Day17

The first part was a simple computer simulation.

For the second one I didn't really succeed on my own...

Sadly in order to find the result I had to look at tips on how to solve it.

Basically each instruction corresponds to a byte in the register A

The register A is basically reduced like `out A%8, A = A/8`, so to build the register A you just have to go backward :
- For the last instruction, check all results for `regA in [0..7]`
- For the one before, check all the results for `regA in [prev*8..prev*8+7]`
- Continue recursively until you get all the numbers

Even though I didn't solve this one on my own I learnt how to analyze the output in order to find results

### Day 18:

Nice and easy day today

Solution is a bit slow so will try to optimize it.

Mainly the second part, I already only look for a path if the current position is in the previous path.

To get the said path I tried with the smallest and the first, it works better with the smallest (less chance to fall on the path => less paths to find)

#### First version :
At first I used a DFS to find the smallest path for both part1 and part2.

```hs
smallestPathFind :: Set (Int, Int) -> Map (Int, Int) Int -> (Int, Int) -> (Int, Int) -> Int -> (Int, Set (Int, Int), Map (Int, Int) Int)
smallestPathFind memory seen pos target nbMoves
  | isOut target pos = (-1, S.empty, seen)
  | pos `M.member` seen && seen M.! pos <= nbMoves = (-1, S.empty, seen)
  | pos `S.member` memory = (-1, S.empty, currSeen)
  | pos == target = (nbMoves, S.singleton target, currSeen)
  | length bestSol /= 0 = (bestMoves, S.insert pos bestPath, seenRight)
  | otherwise = (-1, S.empty, seenRight)
  where
    currSeen = M.insert pos nbMoves seen
    (movesUp, pathUp, seenUp) = smallestPathFind memory currSeen (move pos UP) target (nbMoves + 1)
    (movesLeft, pathLeft, seenLeft) = smallestPathFind memory seenUp (move pos LEFT) target (nbMoves + 1)
    (movesDown, pathDown, seenDown) = smallestPathFind memory seenLeft (move pos DOWN) target (nbMoves + 1)
    (movesRight, pathRight, seenRight) = smallestPathFind memory seenDown (move pos RIGHT) target (nbMoves + 1)

    bestSol = sortOn fst . filter ((/= -1) . fst) $ [(movesUp, pathUp), (movesLeft, pathLeft), (movesDown, pathDown), (movesRight, pathRight)]
    (bestMoves, bestPath) = head bestSol
```

```hs
{- Simple part2 version using previous found path as condition to find a new path -}
sub n mem [] lastPath = (-1, -1)
sub n mem (e : l) lastPath
  | e `S.notMember` lastPath = sub (n + 1) newMem l lastPath
  | currRes == -1 = swap e
  | otherwise = sub (n + 1) newMem l currPath
  where
    newMem = S.insert e mem
    (currRes, currPath, _) = smallestPathFind newMem M.empty (0, 0) (range, range) 0
```

Time : ~25s

#### Second Version :

Then I thought that for the second part any path could do the trick
So instead of the smallest path, I changed the algorithm to return the first path found.
This method didn't work and was actually slower than the previous version.
The time saved by taking the first solution isn't worth the additional number of path to find when a position is in this new bigger path.

```hs
anyPathFind :: Set (Int, Int) -> Map (Int, Int) Int -> (Int, Int) -> (Int, Int) -> Int -> (Int, Set (Int, Int), Map (Int, Int) Int)
anyPathFind memory seen pos target nbMoves
  | isOut target pos = (-1, S.empty, seen)
  | pos `M.member` seen && seen M.! pos <= nbMoves = (-1, S.empty, seen)
  | pos `S.member` memory = (-1, S.empty, currSeen)
  | pos == target = (nbMoves, S.singleton target, currSeen)
  | movesUp /= -1 = (movesUp, S.insert pos pathUp, seenUp)
  | movesLeft /= -1 = (movesLeft, S.insert pos pathLeft, seenLeft)
  | movesDown /= -1 = (movesDown, S.insert pos pathDown, seenDown)
  | movesRight /= -1 = (movesRight, S.insert pos pathRight, seenRight)
  | otherwise = (-1, S.empty, seenRight)
  where
    currSeen = M.insert pos nbMoves seen
    (movesUp, pathUp, seenUp) = anyPathFind memory currSeen (move pos UP) target (nbMoves + 1)
    (movesLeft, pathLeft, seenLeft) = anyPathFind memory seenUp (move pos LEFT) target (nbMoves + 1)
    (movesDown, pathDown, seenDown) = anyPathFind memory seenLeft (move pos DOWN) target (nbMoves + 1)
    (movesRight, pathRight, seenRight) = anyPathFind memory seenDown (move pos RIGHT) target (nbMoves + 1)
```

Time : ~40s

#### Third Version :

Then Raphaël Montes (Sheinxy) made me think about BFS that are actually way faster in this case.
So here is my implementation.

```hs
bfs :: Set (Int, Int) -> (Int, Int) -> Set (Int, Int) -> Set (Int, Int) -> Int -> Int
bfs memory target seen curr acc
  | S.null curr = -1
  | target `S.member` curr = acc
  | otherwise = bfs memory target (S.union seen curr) newCurr (acc + 1)
  where
    notSeen = curr S.\\ seen
    notInWall = notSeen S.\\ memory
    notOut = S.filter (not . isOut target) notInWall
    newCurr = S.fromList . concat . map (\p -> map (move p) [UP, LEFT, RIGHT, DOWN]) $ S.toList notOut
```

```hs
{- Another Simple sub using BFS but not keeping memory of previous paths-}
sub n mem [] = (-1, -1)
sub n mem (e : l)
  | currRes == -1 = swap e
  | otherwise = sub (n + 1) newMem l
  where
    newMem = S.insert e mem
    currRes = bfs newMem (range, range) S.empty (S.singleton (0, 0)) 0
```

Time : ~13s

#### Fourth Version:

By using my two previous best versions I tried to do part2 using a bfs that returns the path.
So instead of calculating the path each time, I only do it when the position would block the previous path found.


```hs
bfs :: Set (Int, Int) -> (Int, Int) -> Set (Int, Int) -> Set (Int, Int) -> Map (Int, Int) (Set (Int, Int)) -> Set (Int, Int)
bfs memory target seen curr paths
  | S.null curr = S.empty
  | target `S.member` curr = paths M.! target
  | otherwise = bfs memory target (S.union seen curr) newCurr newPaths
  where
    notSeen = curr S.\\ seen
    notInWall = notSeen S.\\ memory
    notOut = S.filter (not . isOut target) notInWall

    f2 p (curr', paths') p' = (S.insert p' curr', M.insert p' (S.insert p' (paths M.! p)) paths')
    f1 cp p = foldl (f2 p) cp $ map (move p) [UP, LEFT, RIGHT, DOWN]
    (newCurr, newPaths) = S.foldl f1 (S.empty, M.empty) notOut
```

```hs
part2 :: Int -> Int -> Input -> (Int, Int)
part2 range firstBatch input = sub memory (drop firstBatch input) (findPath memory)
  where
    memory = S.fromList $ take firstBatch input
    findPath m = bfs2 m (range, range) S.empty (S.singleton (0, 0)) (M.singleton (0, 0) S.empty)
    sub mem [] lastPath = (-1, -1)
    sub mem (e : l) lastPath
      | e `S.notMember` lastPath = sub newMem l lastPath
      | S.null currPath = swap e
      | otherwise = sub newMem l currPath
      where
        newMem = S.insert e mem
        currPath = findPath newMem
```

Time : 0.3s

#### Last Version:

After this interesting idea, Raphaël Montes had another one : `dichotomic search` which is my last version

Time : ~0.08s

### Day19:

Today was nice and easy.

First I tried the brute force DFS approach then when I realized it was way too long, I decided to switch to a BFS approach using sets.

This solution worked for part1 but then with Sets, part2 was not possible so I thought of another way using a lookup table for already found solutions.
