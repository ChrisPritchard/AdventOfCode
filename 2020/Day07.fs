module Day07

open Common
open System.IO

let input = 
    File.ReadAllLines("./inputs/day07.txt")
    |> Array.map (fun l -> 
        let bits = l |> splits [|" "; "contain"; "bags"; "bag"; "."; ","|]
        let types = 
            bits |> Array.skip 2 
            |> Array.chunkBySize 3 |> Array.filter (Array.length >> (=) 3) 
            |> Array.map (fun a -> a.[1] + " " + a.[2], int a.[0])
            |> Map.ofArray
        bits.[0] + " " + bits.[1], types)

let part1 () = 
    let rec collector soFar innerBag =
        let next = 
            input 
            |> Array.choose (fun (b, c) -> if Map.containsKey innerBag c then Some b else None)
            |> Array.except soFar
            |> Array.distinct
        if Array.isEmpty next then
            soFar
        else
            let newSet = soFar |> Array.append next
            next |> Array.collect (collector newSet) |> Array.distinct
    Array.length (collector Array.empty "shiny gold")
        

let part2 () =
    let ruleIndex = Map.ofArray input
    let rec collector bagType =
        Map.toArray ruleIndex.[bagType]
        |> Array.sumBy (fun (subType, c) -> c + c * (collector subType))
    collector "shiny gold"