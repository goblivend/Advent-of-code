# Advent of code 2022

No idea what language to do this year with, will start with haskell

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

Was a bit rusty for this restart in haskell, had difficulties downloading GHC, using packages and remembering the syntax
All in all it took me ~1h for something that should have taken 10mins...
Though this year's first is harder than previous years imo
And it didn't help that I tihnk that the subject is wrong ("eightwo" is considered as 82 while I think it should be only 8 since after it's "wo"and not "two"....)

### Day 2:

Summary of this day : it helps to read the subject
After 1h of parsing and getting the algo for the first part, and several minutes of trying to make it work realising that the subject asks for valid games and not invalid games.....
And as if I had not learned anything still didn't read the subject correctly and did stupid mistakes...
Still quite rusty on Haskell lots of going back and forward on the doc...

### Day 3:

Today I managed to read the subject 🥳🥳(edit: well actually no... )
But it did not stop me to do stupid things and have a hard time over easy things
The subject was not as clear as I'd like, it was never mentionned we would have 2 number for a gear... so when my result was not good I went and tried to fix this while in fact it was useless...
Also, this time the fault is on haskell, `groupBy` works by `takeWhile` so you need to have a sorted input... dumb mistake for which wasted ~1h by analysing the input, looking at someone else's results with my input...
on the bright side I temporarily managed to make my vscode lsp work, no idea how or why it's gone now but well....


### Day 4:

Today is finally the day, I read the subject, may this day be remembered.
Also the first time I used the Haskell regex lib `regex-tdfa` spent some time trying to understand how it worked and the parsing of `|` is not nice at all XD.
After that first part was easy enough.
Second part was a bit more tricky,
 - first solution was quite slow : for each cards won I went looking how many I won so the complexity was exponential...
 - Second one is in linear time :
     - I go in reverse order and in an accumulator I store how many copy cards I get for each base card
     - Then instead of doing the calculations many time, I just get that result for the next ones

I also went looking for what the operator `<$>` meant and did a bit of refacto to include it in today's solution

Did a bit of refacto with @Sheinxy's solution and managed to create an improved version that is faster than either one
