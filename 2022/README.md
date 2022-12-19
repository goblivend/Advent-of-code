# Advent of code 2022

This year I will probably try to use Haskell or Lua\
(as well as Bash, Python, Java, JS, C#, C if needed)

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

Was pretty easy to find a solution, did it first in shell, quite happy to have done it this fast.
Did it in Haskell afterward, quite shorter and seems nice when you understand how to use it, `$` is a nice feature

### Day 2:

Was sick so was quite tiring to do it, and clearly took more time than I should have but still managed to do it.
Perhaps not the nicest solution, will try to refactor it

### Day 3:

Was still a bit sick but was ok, managed to do the solution, though I didn't want to download any packages for now so had to copy paste the functions form their source

### Day 4:

Was quite easy, decided to download the package `split` anyway, just saving time quite happy about the solution, not to long.
Though I might try to refactor a bit..

### Day 5:

Was a bit tricky this one. I had a very good idea for an iterative language but didn't quite get one for Haskell at first so had to mess arround in order to find an idea

### Day 6:

Was really straight forward, the second one was clearly obvious, had already a solution by doing the first one.

### Day 7:

This one was quite complicated, didn't manage to think of how to do it in Haskell. Had to do it in python sadly

### Day 8:

At first didn't quite get the good methodology, had to do it first in python in order to do it in haskell, realized half way through the python version what was my mistake.
Otherwise I'm quite happy how the haskell code turned out, I used `where`, lambda functions and list creation

### Day 9 :

This one was quite a mess but I'm quite happy with my solution.
Had luck making it work at first but managed to fix it for any input and now it should work completly fine

### Day 10 :

Was fun, the first part was very easy and the input I use is quite fun since I avoid most the information to only get a list of int without the name of the instructions so quite happy of the input analysis I did.
Had a bit of difficulty trying to understand how the part 2 was working but managed to do it so that's ok.

### Day 11 :

This one was a bit tricky, the input was way more difficult that usual and the thingy with the overflow was a bit hard to figure out (thx raph for the help).
For this one managed to change my implementation to use `data` which makes the code way more readable

### Day 12 :

This one was pathfinding, begun with a dfs but was really a bad idea so went on a bsf instead.
Not quite proud of how part 2 ended, big copypaste with a small modification, will try to change it to a better one
Today I also discovered that there was a debugger in Haskell, will have to look into it, for the moment seems really bad but need to try it.

### Day 13 :

This one was not too hard.
Had to think of a trick to have lists of possibly int or list but managed using `'[' <=> -1` and `']' <=> -2`.
Finding the bugs were a bit tricky with those functions but some `trace` made the trick so it was ok

### Day 14 :

Was fun, even managed to create a pretty print, though the solution is a bit long will try to refactor it.
Managed to pretty print the map and to count according to the values and not to the number of iterations

### Day 15 :

This one was quite tricky, tried to do it the dumb way at first (checking at each point if there was a sensor in range) and this took ages to run.
Raph came in clutch and I saw the idea of using ranges of sight and merging them together so that's what I did and this solution is way faster to solve.
quite happy the way it looks, took idea from raph but still managed to do my own code

### Day 16 :

Didn't manage to fix my answer, even for part 1, I will definetly try to fix it since it only show a result close to the one we want but for the moment I will continue to next Day.

### Day 17 :

For the moment managed to do a nice parsing and uite happy of how it turned out
Now I need to make the rocks move like in a tetris game, and that's getting harder

edit :
Compared to the last two, this one was quite nice the idea of tetris was a bit difficult to visualize the solution at first but then by decomposing every parts I managed to make it.
Though for part 2 I still haven't found a way to shorten the process, I'll have to find a cycle, perhaps using memoisation

### Day 18 :

For this one, the 3D geometric was a bit difficult to manage, I thought I had an idea but then realised it couldn't work so I had quite a lot of thinking to do.
Thanksfully, I think well enough when I'm watching movies so it helped quite a lot.

### Day 19 :

First shot at this one, managed to parse and have a "theoretically working" solution except it's exponential and won't be able to run fast enough, so now I have to limit the results and filter out wrong results
