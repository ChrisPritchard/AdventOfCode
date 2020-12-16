module Day16

open Common
open System.IO

let input = File.ReadAllText "./inputs/day16.txt"

let processed () =
    let parts = input |> splitOn (newline + newline)
    let rules = 
        parts.[0] 
        |> splitOn newline 
        |> Array.map (fun rule -> 
            let p = rule |> splits [": ";"-";" or "]
            p.[0], int p.[1], int p.[2], int p.[3], int p.[4])
    let yours = 
        let line = parts.[1] |> splitOn newline
        line.[1] |> split "," |> Array.map int
    let others =
        parts.[2] |> splitOn newline |> Array.skip 1 |> Array.map (fun line -> line |> split "," |> Array.map int)
    rules, yours, others

let valid v (_, n1, m1, n2, m2) =
    (v >= n1 && v <= m1) || (v >= n2 && v <= m2)

let part1 () =
    let rules, _, others = processed ()
    others 
    |> Array.collect id 
    |> Array.filter (fun v -> rules |> Array.forall (fun r -> not (valid v r)))
    |> Array.sum

let part2 () =
    let rules, yours, others = processed ()
    
    let toTest =
        others 
        |> Array.filter (fun line -> 
            line |> Array.forall (fun v -> 
                rules |> Array.exists (fun r -> valid v r)))
        |> Array.append [|yours|]

    let ruleChecks =
        rules 
        |> Array.map (fun (r, n1, m1, n2, m2) -> 
            let indices = 
                [|0..yours.Length-1|] 
                |> Array.filter (fun i -> 
                    toTest 
                    |> Array.forall (fun v -> 
                        valid v.[i] (r, n1, m1, n2, m2)))
            r, indices)
        |> Array.sortBy (fun (_, c) -> Array.length c)
        |> List.ofArray

    let rec trimmer rules acc =
        match rules with
        | [] -> acc
        | (r, c)::rem ->
            let i = Array.find (fun i -> not (Map.containsKey i acc)) c
            trimmer rem (Map.add i r acc)
            
    let order = trimmer ruleChecks Map.empty
    (1UL, order) ||> Map.fold (fun acc i r -> if r.StartsWith "departure" then uint64 yours.[i] * acc else acc)
        