module Day15

open Common
open System

let processed = readEmbedded "day15" |> Array.map (fun s -> s.ToCharArray() |> Array.map (fun c -> int c - int '0'))

let init () =
    processed |> Array.length |> ignore

let nextTo (y, x) = 
    [|0,-1;-1,0;1,0;0,1|] |> Array.map (fun (dy, dx) -> y + dy, x + dx)

let part1 () =
    let mutable Q = [|0..processed.Length - 1|] |> Array.collect (fun y -> [|0..processed[0].Length - 1|] |> Array.map (fun x -> y, x))

    let mutable dist = Q |> Array.map (fun k -> k, Int32.MaxValue) |> Map.ofArray
    let mutable prev = Map.empty

    dist <- Map.add (0,0) 0 dist
    let mutable found = false
    let target = processed.Length - 1, processed[0].Length - 1

    while Q.Length > 0 && not found do
        let u = Array.minBy (fun o -> dist[o]) Q

        if u = target then found <- true
        else
            Q <- Array.except [|u|] Q

            let neighbours = nextTo u |> Array.filter (fun p -> Array.contains p Q)
            for v in neighbours do
                let (y, x) = v
                let alt = dist[u] + processed[y][x]
                if alt < dist[v] then
                    dist <- Map.add v alt dist
                    prev <- Map.add v u prev

    dist[target]
    
let part2 () =
    0
    
