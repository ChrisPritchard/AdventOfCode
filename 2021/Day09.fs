module Day09

open Common
open System

let processed = readEmbedded "day09" |> Array.map (fun s -> s.ToCharArray() |> Array.map (fun c -> int c - int '0'))

let init () =
    processed |> Array.length |> ignore

let adjacent y x (m: int[][]) = 
    [|-1,0; 0,1; 1,0; 0,-1|]
    |> Array.map (fun (dy, dx) -> y + dy, x + dx)
    |> Array.filter (fun (y, x) -> y >= 0 && x >= 0 && y < Array.length m && x < Array.length m[0])
    |> Array.map (fun (y, x) -> m[y][x])

let part1 () =
    let mutable sum = 0
    for y in [0.. Array.length processed - 1] do
        for x in [0.. Array.length processed[0] - 1] do
            let v = processed[y][x]
            let neighbours = adjacent y x processed
            if Array.forall (fun o -> o > v) neighbours then
                sum <- sum + v + 1
    sum

let part2 () =
    0
