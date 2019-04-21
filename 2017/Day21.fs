(*
--- Day 21: Fractal Art ---

You find a program trying to generate some art. It uses a strange process that involves repeatedly enhancing the detail of an image through a set of rules.

The image consists of a two-dimensional square grid of pixels that are either on (#) or off (.). The program always begins with this pattern:

.#.
..#
###

Because the pattern is both 3 pixels wide and 3 pixels tall, it is said to have a size of 3.

Then, the program repeats the following process:

    If the size is evenly divisible by 2, break the pixels up into 2x2 squares, and convert each 2x2 square into a 3x3 square by following the corresponding enhancement rule.
    Otherwise, the size is evenly divisible by 3; break the pixels up into 3x3 squares, and convert each 3x3 square into a 4x4 square by following the corresponding enhancement rule.

Because each square of pixels is replaced by a larger one, the image gains pixels and so its size increases.

The artist's book of enhancement rules is nearby (your puzzle input); however, it seems to be missing rules. The artist explains that sometimes, one must rotate or flip the input pattern to find a match. (Never rotate or flip the output pattern, though.) Each pattern is written concisely: rows are listed as single units, ordered top-down, and separated by slashes. For example, the following rules correspond to the adjacent patterns:

../.#  =  ..
          .#

                .#.
.#./..#/###  =  ..#
                ###

                        #..#
#..#/..../#..#/.##.  =  ....
                        #..#
                        .##.

When searching for a rule to use, rotate and flip the pattern as necessary. For example, all of the following patterns match the same rule:

.#.   .#.   #..   ###
..#   #..   #.#   ..#
###   ###   ##.   .#.

Suppose the book contained the following two rules:

../.# => ##./#../...
.#./..#/### => #..#/..../..../#..#

As before, the program begins with this pattern:

.#.
..#
###

The size of the grid (3) is not divisible by 2, but it is divisible by 3. It divides evenly into a single square; the square matches the second rule, which produces:

#..#
....
....
#..#

The size of this enhanced grid (4) is evenly divisible by 2, so that rule is used. It divides evenly into four squares:

#.|.#
..|..
--+--
..|..
#.|.#

Each of these squares matches the same rule (../.# => ##./#../...), three of which require some flipping and rotation to line up with the rule. The output for the rule is the same in all four cases:

##.|##.
#..|#..
...|...
---+---
##.|##.
#..|#..
...|...

Finally, the squares are joined into a new grid:

##.##.
#..#..
......
##.##.
#..#..
......

Thus, after 2 iterations, the grid contains 12 pixels that are on.

How many pixels stay on after 5 iterations?

--- Part Two ---

How many pixels stay on after 18 iterations?
*)

module Day21

open Common

let input = System.IO.File.ReadAllLines "./inputs/day21.txt"

let rec rotate = function
    | (_::_)::_ as m -> 
        (List.map List.head m |> List.rev)::(List.map List.tail m |> rotate) 
    | _ -> []

let flipped (pattern: string []) =
    pattern |> Array.map (Seq.rev >> asString)

let rotations (pattern: string []) =
    let listed = pattern |> Seq.map List.ofSeq |> Seq.toList
    [|
        pattern
        rotate listed |> Seq.map asString |> Seq.toArray
        rotate (rotate listed) |> Seq.map asString |> Seq.toArray
        rotate (rotate (rotate listed)) |> Seq.map asString |> Seq.toArray
    |]

let allPossibilities pattern =
    [|
        yield! rotations pattern
        yield! rotations (flipped pattern)
    |] |> Array.distinct

let rules =
    input
    //[|
    //    "../.# => ##./#../..."
    //    ".#./..#/### => #..#/..../..../#..#"
    //|]
    |> Array.collect (fun line -> 
        let parts = split " =>" line
        let outs = split "/" parts.[1]
        let ins = split "/" parts.[0]
        allPossibilities ins |> Array.map (fun p -> p, outs))
    |> Map.ofArray

let partition (array: string []) =
    let dim = array.Length
    if dim % 2 = 0 then
        [|0..dim/2-1|] |> Array.map (fun y ->
            [|0..dim/2-1|] |> Array.map (fun x ->
                let oy, ox = y * 2, x * 2
                [|
                    array.[oy].[ox..ox+1]
                    array.[oy+1].[ox..ox+1]
                |]))
    else
        [|0..dim/3-1|] |> Array.map (fun y ->
            [|0..dim/3-1|] |> Array.map (fun x ->
                let oy, ox = y * 3, x * 3
                [|
                    array.[oy].[ox..ox+2]
                    array.[oy+1].[ox..ox+2]
                    array.[oy+2].[ox..ox+2]
                |]))

let transform (arrays: string [][][]) =
    arrays |> Array.map (fun row -> row |> Array.map (fun chunk -> rules.[chunk]))

let combine (arrays: string [][][]) =
    arrays 
    |> Array.map (fun subArrays ->
        [|0..subArrays.[0].Length - 1|] 
        |> Array.map (fun row -> 
            subArrays 
            |> Array.map (fun subArray -> subArray.[row])
            |> Array.reduce (+)))
    |> Array.collect id

let countOn iterations = 
    let start = 
        [|
            ".#."
            "..#"
            "###"
        |]
    let result = 
        (start, [1..iterations]) 
        ||> List.fold (fun state _ -> 
            state |> partition |> transform |> combine)
    result |> Seq.collect id |> Seq.filter ((=) '#') |> Seq.length

let part1 () =
    countOn 5

let part2 () =
    countOn 18