---
title: 2023
---

No idea what language to do this year with, will start with haskell

- [x] Day 1 (0.020s)
- [x] Day 2 (0.018s)
- [x] Day 3 (0.075s)
- [x] Day 4 (0.081s)
- [x] Day 5 (0.029s)
- [x] Day 6 (0.016s)
- [x] Day 7 (0.037s)
- [x] Day 8 (0.056s)
- [x] Day 9 (0.030s)
- [x] Day 10 (0.204s)
- [x] Day 11 (0.034s)
- [x] Day 12 (3.854s)
- [x] Day 13 (0.030s)
- [x] Day 14 (5.809s)
- [x] Day 15 (0.036s)
- [x] Day 16 (4.205s)
- [x] Day 17 (16.04s)
- [x] Day 18 (0.024s)
- [x] Day 19 (0.095s)
- [x] Day 20 (0.100s)
- [x] Day 21 (0.323s)
- [x] Day 22 (58.59s)
- [x] Day 23 (6.736s)
- [x] Day 24 (0.140s)
- [x] Day 25 (0.033s) (+neato)

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

### Day 15

This day was pretty easy and quite nice.

Though I cannot say it was really interesting, it was mostly "follow this algorithm" and not a lot of thinking involved

### Day 16

This was first day done at ~6am more exactly 6:15am counting the waky waky and the code setup and 3000 so not that bad

### Day 17

This day was quite difficult but after ~8h of coding managed to do it

At first seemed like an annoying type of difficult but actually was quite nice :D

### Day 18

At first managed to make a nice solution using the grid itself like in the examples

But because of part2 had to pass through maths formulas...

### Day 19

Completely forgot to fill this readme that day...

### Day 20

this day was quite annoying, managed to get the first part running but then for the second part.... trouble started

I spent the whole day we basically a working solution, tried to rewrite it like Sheinxy's but still nothing good, I change (after several hours of debugging) the way the algorithm works and like magic it gives the good result but the changes shouldn't have done anything to the result.....

### Day 21

First part was nice and interesting :D

Second part was not so great, once again it was input specific optimisation, on things that didn't even appear on the examples.....

Anyway managed to get a solution from Adam's formulas, and managed to speed up my first part thx to Sheinxy's

And found a bonus approximation algorithm that is almost correct

### Day 22

Forgot...

### Day 23

Well, today's challenge was quite easy to understand and to code dummy solutions

It was quite interesting to go from DFS => BFS => DFS (with constant output concerning the output found) => DFS (with max found when higher found) => BFS (with max found when higher found) => Graph because let's face it that was way too slow...

### Day 24

Today was quite interesting.
I managed to do part1 on my own

Then came part2, I theoretically had a working solution trying for each position and each velocity if they were correct
This one was way too complex and could never have worked
So I tried to do it another way : find every possible combination of position and velocity for x then for each of them the matching y position and velocity in order to find the first z valid position
Then I realised that we were dealing with positions around 1e14... which was wayy to big to try each of them

So, not finding any thing else and not wanting to use z3 SAT solvers, I went to look for another way to solve it

found an interesting one taking advantage on particles having the same speed on some axis, which would mean that the distance between the two rocks had to be a multiple of the relative speed between the rock and the particles on that axis => hence making enough constraints to get only one possible velocity for each axis

Then it was just a matter of getting the starting position with the velocities and 2 particles

### Day 25

The last Day !!

Well was kind of annoying, found several ways of solving the problem but they were way too slow for such an input so in the end,
I just created the dot representation of the graph, used neato to visualise it, find the 3 links and calculate the result....

I will try to optimise and figure out a way to solve it completely but I might not succeed
