module Day05

open Common
open System.IO

let mutable out = 0;

let parse (mem: int[]) value mode =
    if mode = 0 then mem.[value] else value

let text = File.ReadAllText ("./inputs/day05.txt")
let cells = text.Split ',' |> Array.map int

let part1 () =

    out <- 0
    let memory = Array.copy cells

    let ops input = Map.ofList [
        1, (fun pIndex (mem: int[]) (modes: int[]) ->
            mem.[mem.[pIndex + 3]] <- 
                parse mem mem.[pIndex + 1] modes.[0] + 
                parse mem mem.[pIndex + 2] modes.[1]
            pIndex + 4)
        2, (fun pIndex (mem: int[]) (modes: int[]) ->
            mem.[mem.[pIndex + 3]] <- 
                parse mem mem.[pIndex + 1] modes.[0] *
                parse mem mem.[pIndex + 2] modes.[1]
            pIndex + 4)
        3, (fun pIndex (mem: int[]) _ ->
            mem.[mem.[pIndex + 1]] <- input
            pIndex + 2)
        4, (fun pIndex (mem: int[]) (modes: int[]) ->
           out <- parse mem mem.[pIndex + 1] modes.[0]
           pIndex + 2)
    ]

    let withInput = ops 1
    Intcode.run withInput 0 memory |> ignore

    out

let part2 () =
    
    out <- 0
    let memory = Array.copy cells

    let ops input = Map.ofList [
        1, (fun pIndex (mem: int[]) (modes: int[]) ->
            mem.[mem.[pIndex + 3]] <- 
                parse mem mem.[pIndex + 1] modes.[0] + 
                parse mem mem.[pIndex + 2] modes.[1]
            pIndex + 4)
        2, (fun pIndex (mem: int[]) (modes: int[]) ->
            mem.[mem.[pIndex + 3]] <- 
                parse mem mem.[pIndex + 1] modes.[0] *
                parse mem mem.[pIndex + 2] modes.[1]
            pIndex + 4)
        3, (fun pIndex (mem: int[]) _ ->
            mem.[mem.[pIndex + 1]] <- input
            pIndex + 2)
        4, (fun pIndex (mem: int[]) (modes: int[]) ->
           out <- parse mem mem.[pIndex + 1] modes.[0]
           pIndex + 2)
        5, (fun pIndex (mem: int[]) (modes: int[]) ->
            let value = parse mem mem.[pIndex + 1] modes.[0]
            if value > 0 then parse mem mem.[pIndex + 2] modes.[1] else pIndex + 3)
        6, (fun pIndex (mem: int[]) (modes: int[]) ->
            let value = parse mem mem.[pIndex + 1] modes.[0]
            if value = 0 then parse mem mem.[pIndex + 2] modes.[1] else pIndex + 3)
        7, (fun pIndex (mem: int[]) (modes: int[]) ->
            let value1 = parse mem mem.[pIndex + 1] modes.[0]
            let value2 = parse mem mem.[pIndex + 2] modes.[1]
            mem.[mem.[pIndex + 3]] <- if value1 < value2 then 1 else 0
            pIndex + 4)
        8, (fun pIndex (mem: int[]) (modes: int[]) ->
            let value1 = parse mem mem.[pIndex + 1] modes.[0]
            let value2 = parse mem mem.[pIndex + 2] modes.[1]
            mem.[mem.[pIndex + 3]] <- if value1 = value2 then 1 else 0
            pIndex + 4)
    ]

    let withInput = ops 5
    Intcode.run withInput 0 memory |> ignore

    out