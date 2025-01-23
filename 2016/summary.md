# Advent of Code 2016

My solutions for the [Advent of Code competition, Year 2016](https://adventofcode.com/2016)

I did this early 2019, so didn't reach the board obviously :) Just coding practice.

Unlike the other years prior to this (2018, 2015) I haven't put each solution in its own folder as its own project. Instead each is a module called by the central program.fs

2016 is much more complicated than 2015, more akin to what happened in 2018. Still, largely simple with a few exceptions:
  
- Day 11 was hard, largely due to some struggles representing the model. My solution takes five minutes on a powerful computer for part1, which tells me I've expressed it too bluntly. Part 2 uses part 1 to figure out that the time increase is predicatable, then uses that to derive the answer for part 2 (I gave up an earlier brute force attempt after I got no answer after an hour).
- Day 19 had a simple part 1, but its part 2 defied brute force attempts. Ultimately I brute forced the first 100 results, detected a pattern based on the power of three, then created a heuristic to generate the correct answer.
- Day 22 part 2 wasn't obvious to me - I had to check the reddit thread to realise it was pretty simple. Then I was thrown off briefly by an off-by-one error (which was okay, as it said my solution was too low, telling me that I had the right idea just had something mistyped somewhere).
- Day 23 part 2 took a while, though from 2018 I had experience decoding assembly to figure out what was happening. I wrongly thought my initial implementation of toggle was incorrect (I thought it wasn't relative, when it was). As always, careful reading and re-reading of the problem is important.
- Day 25 took longer than it should have. A middle block which does a loop would have worked better I think if I had access to DO..WHILE, but I figured it out after numerous off by 1 errors. Ultimately I solved this by basically replicating most of the code, rather than switching to derived constants (other than the initial constant). Fun!

Overall, harder than 2015 by a lot, but not as hard as 2018. Good times.