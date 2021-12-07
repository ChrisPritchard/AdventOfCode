module Day07

open Common
open System

let processed = readEmbedded "day07" |> Array.head |> split "," |> Array.map int

let init () =
    processed |> Array.length |> ignore

let part1 () =
    let range = [|Array.min processed..Array.max processed|]
    range |> Array.map (fun o -> processed |> Array.sumBy (fun s -> abs (s - o))) |> Array.min

let part2 () =
    let range = [|Array.min processed..Array.max processed|]
    range |> Array.map (fun o -> processed |> Array.sumBy (fun s -> let n = abs (s - o) in (n * (n+1))/2)) |> Array.min
