module Day07

open Common
open System.IO

let input = File.ReadAllLines "./inputs/day07.txt"
    
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
    let map = processed () |> Array.map (fun (b, c) -> b, c |> Array.map fst |> Set.ofArray)
    let rec collector soFar toFind =
        let next = 
            map 
            |> Array.choose (fun (b, c) -> if (Set.intersect toFind c).Count > 0 then Some b else None)
            |> Set.ofArray
        if Set.isEmpty next then soFar 
        else 
            let soFar = Set.union soFar next
            collector soFar next
    let start = Set.empty.Add "shiny gold"
    (collector Set.empty start).Count

let part2 () =
    let ruleIndex = processed () |> Map.ofArray
    let rec collector bagType =
        ruleIndex.[bagType]
        |> Array.sumBy (fun (subType, c) -> c + c * (collector subType))
    collector "shiny gold"