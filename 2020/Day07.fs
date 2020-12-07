module Day07

open Common
open System.IO

let input = File.ReadAllLines("./inputs/day07.txt")
    
let processed () = 
    input
    |> Array.map (fun l -> 
        let bits = l |> splits [|" "; "contain"; "bags"; "bag"; "."; ","|]
        let types = 
            bits |> Array.skip 2 
            |> Array.chunkBySize 3 |> Array.filter (Array.length >> (=) 3) 
            |> Array.map (fun a -> a.[1] + " " + a.[2], int a.[0])
        bits.[0] + " " + bits.[1], types)

let part1 () = 
    let parentMap = 
        processed () 
        |> Array.collect (fun (parent, children) -> 
            children |> Array.map (fun (child, _) -> child, parent))
        |> Array.groupBy fst
        |> Array.map (fun (bag, parents) ->
            let parents = parents |> Array.map snd |> Array.distinct
            bag, parents)
        |> Map.ofArray

    let rec collector soFar current =
        let next = 
            current 
            |> Array.collect (fun b -> Map.tryFind b parentMap |> Option.defaultValue Array.empty)
            |> Array.distinct |> Array.filter (fun b -> not (Set.contains b soFar))
        if Array.isEmpty next then soFar
        else
            let soFar = Array.fold (fun s b -> Set.add b s) soFar next
            collector soFar next

    collector Set.empty [|"shiny gold"|]
    |> Set.count        

let part2 () =
    let ruleIndex = processed () |> Map.ofArray
    let rec collector bagType =
        ruleIndex.[bagType]
        |> Array.sumBy (fun (subType, c) -> c + c * (collector subType))
    collector "shiny gold"