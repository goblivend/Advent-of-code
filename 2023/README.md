# Advent of code 2022

No idea what language to do this year with, will start with haskell

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

### Day 5

Today, I really realised that compared to previous years challenges were way more difficult, had my first real complexity issue.
At first I tried to create maps and access the destination number.. How to tell you that when you have more than a bilion items in a map... it is kind of slow...
So instead I compared using the ranges

But compared to the input I chose to get some of my ranges as `(min, max)` since I don't really need the `range` but only the last accessible number

I did not exactly timed myself but I must have taken 2h30 to do this one 👀 the end is gonna be long if it keeps getting harder, perhaps haskell has not been the best choice..

Nevertheless, I managed to use better imports, Sets, Maps

Also, from now on, I will try to have only 3 functions : `parseInput`, `part1` and `part2` like haskell master @Sheinxy. I feel like these functions would be similar to `public` things in languages like Java, C# and others, for which a good practice is to keep things `private` aas much as possible.

### Day 6

Well today's challenge should have been day1's honestly it was the most straight forward exercise

I'm wondering if it would have been possible to do on paper using only maths

Well Actually did the math version in haskell for it to run sub 1s and was even easier than I though

### Day 7

Today was not too difficult but creating a sort was a bit annoying to be honnest as well as having cards sorted like "AKQJT" and not alphabetically but understandable

On part 2 I spent nearly 45mins on a forgotten `sort` which lead to Jockers not beeing assigned to the right group....

Except from that the challenge was nice

### Day 8

Today was quite hard and not really in the interesting way...

For the first part bruteforce was ok and fast enough.

For the second part, bruteforce was impossible, and actually the only way to solve it was to make assemptions about the input and that was not a nice part...

Like I can give you inputs that match the requirements in the subject and that would be unsolvable with most solutions created for today...

Finally, with some help about what assemptions I could do, I managed to have a working solution

And then I realised from @Sheinxy's answer that some big part of mine (in which there was a bug but I knew it would not matter for this input) was actually a function available in the language...

So I managed to clean up most of it (the first version is still available as first commit of the day)

### Day 9

Today was really easy and straight forward, doing it in with recursivity haskell really paid off.

### Day 10

This day was quite difficult and long to do but managed to find a way
At first was quite long since I was using `elem` in lists but now, using `member` in sets it's a way better time

### Day 11

The day was quite nice

Just for part two had stupid mistake and did for 1_000_001 instead of 1_000_000

### Day 12

This day was quite difficult.

First I tried to brute force (knowing it would not work for part2 but would get me there). As expected, part1 worked then part2 didn't

Then @Sheinxy came along and talked about Memoisation.

Not succeeding in the "smart" way, I went and check on his solution and managed to implement it in my code

### Day 13

Nice to be back on nice easy problems

Found the perfect function `transpose` that allows me to call the same function for both vertical and horizontal lines which is pretty neat

And managed to get a nice code

### Day 14

This day was actually not that difficult, even though I had a hard time with part2

Actually at first didn't know why I didn't have the good result nor where was the mistake, so went looking for my result using someone else's code to figure out what was wrong

... I didn't copy the right number and was looking for the wrong result for ~45 mins.....

Once I realised that my solution miraculously worked... XDD
