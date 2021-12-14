module Day14

open Common
open System

let processed = 
    let a = readEmbedded "day14" 
    a[0].ToCharArray(), a[1..] |> Array.map (fun s -> split " ->" s |> fun b -> b[0].ToCharArray(), b[1][0]) |> Map.ofArray

let init () =
    snd processed |> Map.count |> ignore

let part1 () =
    let apply_step current =
        current
        |> Array.windowed 2
        |> Array.collect (fun p -> [|p[0]; (snd processed)[p]|])
        |> fun r -> Array.append r [|current[current.Length - 1]|]

    let res = [1..10] |> List.fold (fun c _ -> apply_step c) (fst processed) |> Array.groupBy id |> Array.map (fun (_, a) -> a.Length) |> Array.sort
    res[res.Length - 1] - res[0]
    
let part2 () =
    let rules = snd processed
    let apply_step current =
        let mutable next = Map.empty
        for (k, v) in Map.toArray current do
            match Map.tryFind k rules with
            | None -> next <- Map.add k v next
            | Some c -> 
                let n1 = [|k[0];c|]
                let n2 = [|c;k[1]|]
                let v1 = match Map.tryFind n1 next with None -> v | Some n -> n + v
                let v2 = match Map.tryFind n2 next with None -> v | Some n -> n + v
                next <- next |> Map.add n1 v1 |> Map.add n2 v2
        next

    let start = (fst processed) |> Array.windowed 2 |> Array.groupBy id |> Array.map (fun (a, v) -> a, int64 v.Length) |> Map.ofArray
    let folded = [1..40] |> List.fold (fun c _ -> apply_step c) start |> Map.toArray
    let res = folded |> Array.map (fun (k, v) -> k[1], v) |> Array.groupBy fst |> Array.map (fun (k, v) -> k, Array.map snd v |> Array.sum) |> Array.sortBy snd
    (snd (res[res.Length - 1])) - (snd res[0])
    
