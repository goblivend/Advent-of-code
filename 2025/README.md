---
title: 2025
---

This year will do it in Haskell, but might try few days in Rust as well

- [x] [Day 1](./Day01/README.md) (0.018s)
- [x] [Day 2](./Day02/README.md) (0.912s)
- [x] [Day 3](./Day03/README.md) (0.038s)
- [x] [Day 4](./Day04/README.md) (1.032s)
- [x] [Day 5](./Day05/README.md) (0.014s)
- [x] [Day 6](./Day06/README.md) (0.024s)
- [x] [Day 7](./Day07/README.md) (0.021s)
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

Quite happy of the simplification I managed to do, next step would be to try and create those the first time and not using a temporary version

[Code explanation](./Day01/README.md)

### Day 2:

Today first solution was quite intuitive, then for the second one I tried different ideas to try and have a clean solution such as using Regex but didn't manage to find a clean solution.

So I did it by hand:

[Code explanation](./Day02/README.md)


### Day 3:

Today was interesting.

I had to remember a few things about number and once I did I felt stupid not to have thought of it earlier.

[Code explanation](./Day03/README.md)

### Day 4:

Today was quite nice and after part 1 I just adapted a little bit part 1 to actually remove the rolls and that was most of the work for this part.

[Code explanation](./Day04/README.md)

### Day 5:

Today was the first day with an impossible solution, the first version of the second exercise crashed so I had to think a bit and make a cleaner version before solving the problem.

[Code explanation](./Day05/README.md)

### Day 6:

Today, the puzzles were very simple, the actual thinking was the parsing.

At first I just decided to split on the spaces which worked for part1 but then I ended up removing important information for part2, so I had to adapt the parsing.

[Code explanation](./Day06/README.md)

### Day 7:

Today needed the some sort of memoization, instead, I decided to limit the number of calls to the strict minimum

[Code explanation](./Day07/README.md)
