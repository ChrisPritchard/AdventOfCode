module Day12

open Common
open System

let processed = readEmbedded "day12" 

let init () =
    processed |> Array.length |> ignore

let edges () =
    processed
    |> Array.collect (split "-" >> fun a -> [|a[0],a[1]; a[1],a[0]|])
    |> Array.groupBy fst
    |> Array.map (fun (k, a) -> k, Array.map snd a)
    |> Map.ofArray

let part1 () =
    let edges = edges ()        

    let rec finder current visited =
        if current = "end" then
            [|visited|]
        else
            let next = edges[current] |> Array.filter (fun (p: string) -> Char.IsUpper p[0] || not (Array.contains p visited))
            if Array.isEmpty next then
                Array.empty
            else
                next |> Array.collect (fun n -> finder n (Array.append visited [|n|]))
    
    finder "start" [|"start"|] |> Array.length

let part2 () =
    let edges = edges ()        

    let rec finder current visited doubled =
        if current = "end" then
            [|visited|]
        else
            let next = 
                edges[current] 
                |> Array.filter (fun (p: string) -> p <> "start" && (Char.IsUpper p[0] || not (Array.contains p visited) || not doubled))
                |> Array.map (fun (p: string) -> if Char.IsLower p[0] && Array.contains p visited then p, true else p, doubled)
            if Array.isEmpty next then
                Array.empty
            else
                next |> Array.collect (fun (n, doubled) -> finder n (Array.append visited [|n|]) doubled)
    
    finder "start" [|"start"|] false |> Array.length

    
