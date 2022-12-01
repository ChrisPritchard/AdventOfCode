module Day01

open Common
open System

let part1And2 () =
    let lines = Seq.append (readEmbeddedRaw "day01") (Seq.singleton "")
    let (first, second, third, _) =
        ((0, 0, 0, 0), lines)
        ||> 
            Seq.fold (fun (f, s, t, c) (v: string) ->
                if String.IsNullOrWhiteSpace v then
                    if f = 0 then c, s, t, 0
                    elif s = 0 then f, c, t, 0
                    elif t = 0 then f, s, c, 0
                    elif c > f then c, f, s, 0
                    elif c > s then f, c, s, 0
                    elif c > t then f, s, c, 0
                    else f, s, t, 0
                else
                    f, s, t, c + int v) 
    first, first + second + third

// original

// let processed = (readEmbeddedRaw "day01").Split('\n')

// let init () =
//     processed |> Array.length |> ignore

// let sumElves list =
//     let acc, rem =
//         Array.fold (fun (acc: int list, curr: int) (v: string) ->
//             if String.IsNullOrWhiteSpace v then
//                 curr::acc, 0
//             else
//                 acc, (int v) + curr) ([], 0) processed
//     rem::acc

// let part1 () =
//     sumElves processed |> List.max

// let part2 () =
//     sumElves processed |> List.sortByDescending id |> List.take 3 |> List.sum
