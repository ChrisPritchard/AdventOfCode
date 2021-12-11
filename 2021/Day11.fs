module Day11

open Common
open System

let processed = readEmbedded "day11"

let init () =
    processed |> Array.length |> ignore

let positions = 
    [|0..processed.Length - 1|] 
    |> Array.collect (fun y -> 
        [|0..processed[0].Length - 1|] 
        |> Array.map (fun x -> y, x))

let findNeighbours y x =
    [|-1,-1;0,-1;1,-1;-1,0;1,0;-1,1;0,1;1,1|]
    |> Array.map (fun (dy, dx) -> y + dy, x + dx)
    |> Array.filter (fun (dy, dx) -> dy >= 0 && dx >= 0 && dy < 10 && dx < 10)
let neighbours =
    positions |> Array.map (fun (y, x) -> (y, x), findNeighbours y x) |> Map.ofArray

let part1 () =
    let octi = processed |> Array.map (fun s -> s |> Seq.map (fun c -> int c - int '0') |> Array.ofSeq)
    let oct y x = octi[y][x]
    let bump y x = octi[y][x] <- oct y x + 1

    let mutable flashes = 0
    for i in [|1..100|] do
        for (y, x) in positions do
            bump y x

        let mutable flashesFound = true
        while flashesFound do
            flashesFound <- false
            for (y, x) in positions do
                if oct y x = 10 then
                    bump y x
                    flashesFound <- true
                    for ny, nx in neighbours[y,x] do
                        if oct ny nx < 10 then
                            bump ny nx

        for (y, x) in positions do
            if oct y x > 9 then
                flashes <- flashes + 1
                octi[y][x] <- 0

    flashes

let part2 () =
    0
