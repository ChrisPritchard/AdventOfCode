module Day11

open System.IO

let input = File.ReadAllLines "./inputs/day11.txt"

let processed () =
    input |> Array.map Seq.toArray

let part1 () =
    processed ()

let part2 () =
    0