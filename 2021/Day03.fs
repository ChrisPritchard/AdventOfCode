module Day03

open Common
open System

let processed = readEmbedded "day03"

let init () =
    processed |> Array.length |> ignore

let part1 () =
    let gammaS = 
        processed 
        |> Array.collect (fun s -> s.ToCharArray() |> Array.indexed)
        |> Array.groupBy fst
        |> Array.map 
            (fun (_, chars) -> 
                chars |> Array.countBy (snd) |> Map.ofArray)
        |> Array.map
            (fun counts -> if counts['0'] > counts['1'] then '0' else '1')
        |> asString
    let gamma = Convert.ToInt32 (gammaS, 2)
    let epsilonS = 
        processed 
        |> Array.collect (fun s -> s.ToCharArray() |> Array.indexed)
        |> Array.groupBy fst
        |> Array.map 
            (fun (_, chars) -> 
                chars |> Array.countBy (snd) |> Map.ofArray)
        |> Array.map
            (fun counts -> if counts['0'] < counts['1'] then '0' else '1')
        |> asString
    let episilon = Convert.ToInt32 (epsilonS, 2)
    gamma * episilon

let part2 () =
    0
