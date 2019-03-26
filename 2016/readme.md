# Advent of Code 2016

My solutions for the [Advent of Code competition, Year 2016](https://adventofcode.com/2016)

I did this early 2019, so didn't reach the board obviously :) Just coding practice.

Unlike the other years prior to this (2018, 2015) I haven't put each solution in its own folder as its own project. Instead each is a module called by the central program.fs

2016 is much more complicated than 2015, more akin to what happened in 2018. Still, largely simple with a few exceptions:
  
- Day 11 was hard, largely due to some struggles representing the model. My solution takes five minutes on a powerful computer for part1, which tells me I've expressed it too bluntly. Part 2 uses part 1 to figure out that the time increase is predicatable, then uses that to derive the answer for part 2 (I gave up an earlier brute force attempt after I got no answer after an hour).
- Day 19 had a simple part 1, but its part 2 defied brute force attempts. Ultimately I brute forced the first 100 results, detected a pattern based on the power of three, then created a heuristic to generate the correct answer.