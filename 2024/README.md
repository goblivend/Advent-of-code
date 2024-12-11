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
- [ ] Day 10
- [ ] Day 11
- [ ] Day 12
- [ ] Day 13
- [ ] Day 14
- [ ] Day 15
- [ ] Day 16
- [ ] Day 17
- [ ] Day 18
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
