module Day03

open Common
open System

let processed = readEmbedded "day03"

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
