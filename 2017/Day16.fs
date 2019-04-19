(*
--- Day 16: Permutation Promenade ---

You come upon a very unusual sight; a group of programs here appear to be dancing.

There are sixteen programs in total, named a through p. They start by standing in a line: a stands in position 0, b stands in position 1, and so on until p, which stands in position 15.

The programs' dance consists of a sequence of dance moves:

    Spin, written sX, makes X programs move from the end to the front, but maintain their order otherwise. (For example, s3 on abcde produces cdeab).
    Exchange, written xA/B, makes the programs at positions A and B swap places.
    Partner, written pA/B, makes the programs named A and B swap places.

For example, with only five programs standing in a line (abcde), they could do the following dance:

    s1, a spin of size 1: eabcd.
    x3/4, swapping the last two programs: eabdc.
    pe/b, swapping programs e and b: baedc.

After finishing their dance, the programs end up in order baedc.

You watch the dance for a while and record their dance moves (your puzzle input). In what order are the programs standing after their dance?
*)

module Day16

open Common

let input = 
    split "," (System.IO.File.ReadAllText "./inputs/day16.txt")
    |> Array.map (split "/")

let part1 () = 
    let dancers = "abcdefghijklmnop" |> Seq.toArray
    let apply (dancers: char []) (move: string []) =
        match move.[0].[0] with
        | 's' -> 
            let count = int (asString move.[0].[1..])
            Array.append dancers.[dancers.Length - count..] dancers.[0..dancers.Length - count - 1]
        | 'x' ->
            let pos1 = int move.[0].[1..]
            let pos2 = int move.[1]
            let copy = Array.copy dancers
            copy.[pos1] <- dancers.[pos2]
            copy.[pos2] <- dancers.[pos1]
            copy
        | _ -> 
            let char1 = move.[0].[1]
            let char2 = move.[1].[0]
            dancers |> Array.map (fun c -> if c = char1 then char2 elif c = char2 then char1 else c)

    input
    |> Array.fold apply dancers
    |> asString

let part2 () =
    0