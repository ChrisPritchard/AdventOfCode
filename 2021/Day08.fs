module Day08

open Common
open System

let processed = readEmbedded "day08" |> Array.map (fun l -> let parts = split "|" l in split " " parts[0], split " " parts[1])

let init () =
    processed |> Array.length |> ignore

let part1 () =
    processed
    |> Array.collect snd
    |> Array.countBy String.length
    |> Array.sumBy (fun (l, c) -> if l = 2 || l = 4 || l = 3 || l = 7 then c else 0)

let part2 () =
    0
