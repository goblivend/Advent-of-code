# Advent of code 2024

This year will do it in Haskell, but might try few days in Rust as well

- [x] Day 1
- [x] Day 2
- [x] Day 3
- [x] Day 4
- [x] Day 5
- [ ] Day 6
- [ ] Day 7
- [ ] Day 8
- [ ] Day 9
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
