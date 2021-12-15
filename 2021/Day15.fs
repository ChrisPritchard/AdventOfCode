module Day15

open Common
open System

let processed = readEmbedded "day15" |> Array.map (fun s -> s.ToCharArray() |> Array.map (fun c -> int c - int '0'))

let init () =
    processed |> Array.length |> ignore

let height = processed.Length
let width = processed[0].Length

let part1 () =
    let mutable edges = [|0, 0|]
    let mutable dists = Map.empty |> Map.add (0,0) 0
    let mutable visited = Set.empty
    
    let mutable found = false
    let target = height - 1, width - 1

    let getRisk (y, x) = processed[y][x]
    let nextTo (y, x) = 
        [|0,-1;-1,0;1,0;0,1|] 
        |> Array.map (fun (dy, dx) -> y + dy, x + dx) 
        |> Array.filter (fun (y, x) -> y >= 0 && y < height && x >= 0 && x < width)
    
    while not found do
        let next = edges |> Array.minBy (fun e -> dists[e])
        if next = target then found <- true
        else
            visited <- Set.add next visited
            edges <- edges |> Array.except [|next|]

            let neighbours = nextTo next |> Array.filter (fun p -> not (Set.contains p visited))
            edges <- edges |> Array.append neighbours

            for n in neighbours do
                let alt = dists[next] + getRisk n
                match Map.tryFind n dists with
                | Some v when alt < v ->
                    dists <- Map.add n alt dists
                | None ->
                    dists <- Map.add n alt dists
                | _ -> ()

    dists[target]
    
let part2 () =
    let mutable edges = [|0, 0|]
    let mutable dists = Map.empty |> Map.add (0,0) 0
    let mutable visited = Set.empty
    
    let mutable found = false
    let target = height * 5 - 1, width * 5 - 1

    let getRisk (y, x) = 
        let raw = processed[y % height][x % width]
        let my = y / height
        let mx = x / width
        let n = (raw + my + mx)
        if n > 9 then n - 9
        else n
    let nextTo (y, x) = 
        [|0,-1;-1,0;1,0;0,1|] 
        |> Array.map (fun (dy, dx) -> y + dy, x + dx) 
        |> Array.filter (fun (y, x) -> y >= 0 && y < height * 5 && x >= 0 && x < width * 5)
    
    while not found do
        let next = edges |> Array.minBy (fun e -> dists[e])
        if next = target then found <- true
        else
            visited <- Set.add next visited
            edges <- edges |> Array.except [|next|]

            let neighbours = nextTo next |> Array.filter (fun p -> not (Set.contains p visited))
            edges <- edges |> Array.append neighbours

            for n in neighbours do
                let alt = dists[next] + getRisk n
                match Map.tryFind n dists with
                | Some v when alt < v ->
                    dists <- Map.add n alt dists
                | None ->
                    dists <- Map.add n alt dists
                | _ -> ()

    dists[target]
    
