module Day01

open Common

let processed = readEmbedded "day01" |> Array.map int

let part1 () =
    processed
    |> Array.windowed 2
    |> Array.filter (fun set -> set[1] > set[0])
    |> Array.length

let part2 () =
    processed
    |> Array.windowed 3
    |> Array.map Array.sum
    |> Array.windowed 2
    |> Array.filter (fun set -> set[1] > set[0])
    |> Array.length