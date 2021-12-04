module Day04

open Common
open System

let processed = readEmbedded "day04"

let init () =
    processed |> Array.length |> ignore

let part1 () =
    let rates = 
        processed 
        |> Array.collect (fun s -> s.ToCharArray() |> Array.indexed)
        |> Array.groupBy fst
        |> Array.map 
            (fun (_, chars) -> 
                chars |> Array.countBy (snd) |> Map.ofArray)
        |> Array.fold 
            (fun (gamma, epsilon) counts -> 
                if counts['0'] > counts['1'] then 
                    gamma + "0", epsilon + "1" 
                else 
                    gamma + "1", epsilon + "0")
            ("", "")
    Convert.ToInt32 (fst rates, 2) * Convert.ToInt32 (snd rates, 2)

let part2 () =
    0
