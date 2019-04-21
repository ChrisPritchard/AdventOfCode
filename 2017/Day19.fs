﻿(*
--- Day 19: A Series of Tubes ---

Somehow, a network packet got lost and ended up here. It's trying to follow a routing diagram (your puzzle input), but it's confused about where to go.

Its starting point is just off the top of the diagram. Lines (drawn with |, -, and +) show the path it needs to take, starting by going down onto the only line connected to the top of the diagram. It needs to follow this path until it reaches the end (located somewhere within the diagram) and stop there.

Sometimes, the lines cross over each other; in these cases, it needs to continue going the same direction, and only turn left or right when there's no other option. In addition, someone has left letters on the line; these also don't change its direction, but it can use them to keep track of where it's been. For example:

     |          
     |  +--+    
     A  |  C    
 F---|----E|--+ 
     |  |  |  D 
     +B-+  +--+ 

Given this diagram, the packet needs to take the following path:

    Starting at the only line touching the top of the diagram, it must go down, pass through A, and continue onward to the first +.
    Travel right, up, and right, passing through B in the process.
    Continue down (collecting C), right, and up (collecting D).
    Finally, go all the way left through E and stopping at F.

Following the path to the end, the letters it sees on its path are ABCDEF.

The little packet looks up at you, hoping you can help it find the way. What letters will it see (in the order it would see them) if it follows the path? (The routing diagram is very wide; make sure you view it without line wrapping.)

--- Part Two ---

The packet is curious how many steps it needs to go.

For example, using the same routing diagram from the example above...

     |          
     |  +--+    
     A  |  C    
 F---|--|-E---+ 
     |  |  |  D 
     +B-+  +--+ 

...the packet would go:

    6 steps down (including the first line at the top of the diagram).
    3 steps right.
    4 steps up.
    3 steps right.
    4 steps down.
    3 steps right.
    2 steps up.
    13 steps left (including the F it stops on).

This would result in a total of 38 steps.

How many steps does the packet need to go?
*)

module Day19

open Common

let input = 
    System.IO.File.ReadAllLines "./inputs/day19.txt" |> array2D

let rec follower x y last dir acc count =
    match input.[y, x] with
    | ' ' -> acc, count
    | '|' when last = '|' ->
        follower x (y + dir) last dir acc (count + 1)
    | '|' when last = '-' ->
        follower (x + dir) y last dir acc (count + 1)
    | '-' when last = '-' ->
        follower (x + dir) y last dir acc (count + 1)
    | '-' when last = '|' ->
        follower x (y + dir) last dir acc (count + 1)
    | '+' when last = '|' ->
        if input.[y, x - 1] = '-' then follower (x - 1) y '-' -1 acc (count + 1)
        else follower (x + 1) y '-' 1 acc (count + 1)
    | '+' when last = '-' ->
        if input.[y - 1, x] = '|' then follower x (y - 1) '|' -1 acc (count + 1)
        else follower x (y + 1) '|' 1 acc (count + 1)
    | c when System.Char.IsLetter c && last = '|' ->
        follower x (y + dir) last dir (c::acc) (count + 1)
    | c when System.Char.IsLetter c && last = '-' ->
        follower (x + dir) y last dir (c::acc) (count + 1)
    | _ -> failwith "unrecognised tile"
    
let part1 () =
    follower 99 0 '|' 1 [] 0 |> fst |> List.rev |> asString

let part2 () =
    follower 99 0 '|' 1 [] 0 |> snd