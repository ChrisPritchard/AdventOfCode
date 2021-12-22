module Day22

open Common
open System

let processed = readEmbedded "day22"

let init () =
    processed |> Array.length |> ignore

let coords = processed |> Array.map (fun s -> 
    s.Replace("on", "1").Replace("off","0") |> split " x=.,yz" |> Array.map int)

let part1 () =
    coords

let part2 () =
    0