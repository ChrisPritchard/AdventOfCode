(*
--- Day 11: Hex Ed ---

Crossing the bridge, you've barely reached the other side of the stream when a program comes up to you, clearly in distress. "It's my child process," she says, "he's gotten lost in an infinite grid!"

Fortunately for her, you have plenty of experience with infinite grids.

Unfortunately for you, it's a hex grid.

The hexagons ("hexes") in this grid are aligned such that adjacent hexes can be found to the north, northeast, southeast, south, southwest, and northwest:

  \ n  /
nw +--+ ne
  /    \
-+      +-
  \    /
sw +--+ se
  / s  \

You have the path the child process took. Starting where he started, you need to determine the fewest number of steps required to reach him. (A "step" means to move from the hex you are in to any adjacent hex.)

For example:

    ne,ne,ne is 3 steps away.
    ne,ne,sw,sw is 0 steps away (back where you started).
    ne,ne,s,s is 2 steps away (se,se).
    se,sw,se,sw,sw is 3 steps away (s,s,sw).

--- Part Two ---

How many steps away is the furthest he ever got from his starting position?
*)

module Day11

open Common

let input = split "," <| System.IO.File.ReadAllText "./inputs/day11.txt"

// using cubic coords from here: https://www.redblobgames.com/grids/hexagons/
type Hex = Cube of x:int * y:int * z:int

let next (Cube (x, y, z)) dir =
    match dir with
    | "n" -> Cube (x, y + 1, z - 1)
    | "ne" -> Cube (x + 1, y, z - 1)
    | "se" -> Cube (x + 1, y - 1, z)
    | "s" -> Cube (x, y - 1, z + 1)
    | "sw" -> Cube (x - 1, y, z + 1)
    | "nw" -> Cube (x - 1, y + 1, z)
    | _ -> failwith "invalid direction"

let distance (Cube (x, y, z)) =
    (abs x + abs y + abs z) / 2

let part1 () =
    distance (Array.fold next (Cube (0, 0, 0)) input)

let part2 () =
    let mapFold = fun current dir -> let res = next current dir in distance res, res
    Array.mapFold mapFold (Cube (0, 0, 0)) input |> fst |> Array.max