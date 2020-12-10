module Day10

open System
open System.IO
open Common

let input = File.ReadAllLines "./inputs/day10.txt"

let processed () = 
    let raw = input |> Array.map int |> Array.sortBy id
    Array.concat 
        [| 
            [|0|] 
            raw
            [|raw.[Array.length raw - 1]+3|] 
        |]

let part1 () =
    processed ()
    |> Array.windowed 2
    |> Array.map (fun a -> a.[1] - a.[0])
    |> Array.groupBy id
    |> Array.map (fun (k, v) -> k, Array.length v)
    |> Map.ofArray
    |> fun m -> m.[1] * m.[3]

let part2 () =
    let processed = 
        processed () 
        |> Array.windowed 2
        |> Array.map (fun a -> string (a.[1] - a.[0]))
        |> String.concat ""
    processed
    |> splitOn "11"
