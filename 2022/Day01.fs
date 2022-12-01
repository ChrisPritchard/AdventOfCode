module Day01

open Common
open System

let processed = (readEmbeddedRaw "day01").Split('\n')

let init () =
    processed |> Array.length |> ignore

let sumElves list =
    let acc, rem =
        Array.fold (fun (acc: int list, curr: int) (v: string) ->
            if String.IsNullOrWhiteSpace v then
                curr::acc, 0
            else
                acc, (int v) + curr) ([], 0) processed
    rem::acc

let part1 () =
    sumElves processed |> List.max

let part2 () =
    sumElves processed |> List.sortByDescending id |> List.take 3 |> List.sum
