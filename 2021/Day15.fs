module Day15

open Common
open System

let processed = readEmbedded "day15" |> Array.map (fun s -> s.ToCharArray() |> Array.map (fun c -> int c - int '0'))

let init () =
    processed |> Array.length |> ignore

let part1 () =
    Int64.MaxValue
    
let part2 () =
    0
    
