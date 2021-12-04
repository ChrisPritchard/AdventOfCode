module Day03

open Common
open System

let processed = readEmbedded "day03"

let init () =
    processed |> Array.length |> ignore

let part1 () =
    let rowlen = processed[0].Length
    let counter = Array.zeroCreate rowlen
    for p in processed do
        for i = 0 to rowlen - 1 do
            if p[i] = '1' then
                counter[i] <- counter[i] + 1
    let mutable gamma = 0
    let mutable epsilon = 0
    let mid = processed.Length / 2
    for i = 0 to rowlen - 1 do
        if counter[i] >= mid then
            gamma <- gamma*2 + 1
            epsilon <- epsilon*2
        else
            gamma <- gamma*2
            epsilon <- epsilon*2 + 1
    gamma * epsilon

let part2 () =
    let rates index (numbers: string[]) = 
        numbers 
        |> Array.groupBy (fun s -> s[index])
        |> fun groups ->
            if Array.length groups = 1 then
                snd groups[0], snd groups[0]
            else
                let m = Map.ofArray groups
                if Array.length m['0'] = Array.length m['1'] then
                    m['1'], m['0']
                else if Array.length m['1'] > Array.length m['0'] then
                    m['1'], m['0']
                else
                    m['0'], m['1']
    let rec finder index numbers (filter: string[] * string[] -> string[]) =
        match numbers with
        | [|n|] -> Convert.ToInt32 (n, 2)
        | _ -> 
            let next = filter (rates index numbers)
            finder (index + 1) next filter
    finder 0 processed (fst) * finder 0 processed (snd)
