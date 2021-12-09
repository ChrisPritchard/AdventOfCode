module Day09

open Common
open System

let processed = readEmbedded "day09" |> Array.map (fun s -> s.ToCharArray() |> Array.map (fun c -> int c - int '0'))

let init () =
    processed |> Array.length |> ignore

let part1 () =
    let adjacent y x = 
        [|-1,0; 0,1; 1,0; 0,-1|]
        |> Array.map (fun (dy, dx) -> y + dy, x + dx)
        |> Array.filter (fun (y, x) -> y >= 0 && x >= 0 && y < Array.length processed && x < Array.length processed[0])
        |> Array.map (fun (y, x) -> processed[y][x])

    let mutable sum = 0
    for y in [0.. Array.length processed - 1] do
        for x in [0.. Array.length processed[0] - 1] do
            let v = processed[y][x]
            let neighbours = adjacent y x
            if Array.forall (fun o -> o > v) neighbours then
                sum <- sum + v + 1
    sum

let part2 () =
    let adjacent (y, x) = 
        [|-1,0; 0,1; 1,0; 0,-1|]
        |> Array.map (fun (dy, dx) -> y + dy, x + dx)
        |> Array.filter (fun (y, x) -> y >= 0 && x >= 0 && y < Array.length processed && x < Array.length processed[0])

    let v (y, x) = processed[y][x]

    let crawler y x =
        let rec expander acc edges =
            let next = edges |> Array.collect adjacent |> Array.distinct |> Array.except acc |> Array.filter (fun p -> v p <> 9)
            if Array.isEmpty next then
                Set.ofArray acc
            else
                expander (Array.append next acc) next
        let mutable soFar = Set.empty
        let mutable total = Array.empty
        for y in [0.. Array.length processed - 1] do
            for x in [0.. Array.length processed[0] - 1] do
                if not (Set.contains (y, x) soFar) && processed[y][x] <> 9 then
                    let basin = expander [|y,x|] [|y, x|]
                    soFar <- Set.union soFar basin
                    total <- Array.append total [|Set.count basin|]
        Array.sortDescending total |> fun a -> Array.reduce (*) a[0..2]
    
    crawler 0 0
