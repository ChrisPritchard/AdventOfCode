
open System.IO

[<EntryPoint>]
let main argv =
    
    let input = File.ReadAllLines "input.txt"
    let containers = input |> Array.map int |> Array.mapi (fun i c -> i, c) |> Array.toList
    
    let rec combos soFar containers remaining = 
        containers 
        |> List.filter (fun (_, c) -> remaining - c >= 0)
        |> List.collect (fun (i, c) ->
            if remaining - c = 0 then [(i, c)::soFar |> List.sortBy fst]
            else 
                let others = containers |> List.filter ((<>) (i, c))
                combos ((i, c)::soFar) others (remaining - c)) |> List.distinct

    let allCombos = combos [] containers 150
    let part1 = List.length allCombos
    printfn "part 1: %i" part1

    let part2 = allCombos |> List.countBy List.length |> List.minBy fst |> snd
    printfn "part 2: %i" part2

    0
