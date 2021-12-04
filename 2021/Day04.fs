module Day04

open Common
open System

let processed = readEmbedded "day04"

let init () =
    processed |> Array.length |> ignore

let part1 () =
    let numbers = split "," processed[0] |> List.ofArray

    let mutable boards = []
    for i in [1..5..processed.Length-1] do
        let board = processed[i..i+4] |> Array.map (split " ")
        boards <- board::boards

    let wins marked (board: string[][]) =
        let mutable victory = false
        for y = 0 to 4 do
            let mutable unmarked = false
            for x = 0 to 4 do 
                if not (Set.contains (board[y][x]) marked) then
                    unmarked <- true
            if not unmarked then
                victory <- true
        for x = 0 to 4 do 
            let mutable unmarked = false
            for y = 0 to 4 do    
                if not (Set.contains (board[y][x]) marked) then
                    unmarked <- true
            if not unmarked then
                victory <- true
        victory

    let unmarked marked board = 
        board |> Array.collect id |> Array.sumBy (fun s -> if Set.contains s marked then 0 else int s)

    let rec play soFar (numbers: string list) boards =
        match numbers with
        | next::rem ->
            let nextMarked = Set.add next soFar
            let rec testBoards boards =
                match boards with 
                | b::rest ->
                    if wins nextMarked b then
                        Some ((unmarked nextMarked b) * int next)
                    else
                        testBoards rest
                | _ -> None
            match testBoards boards with
            | Some n -> n
            | _ -> play nextMarked rem boards
        | _ -> failwith "shouldn't be reached"

    play Set.empty numbers boards

let part2 () =
    let numbers = split "," processed[0] |> List.ofArray

    let mutable boards = []
    for i in [1..5..processed.Length-1] do
        let board = processed[i..i+4] |> Array.map (split " ")
        boards <- board::boards

    let wins marked (board: string[][]) =
        let mutable victory = false
        for y = 0 to 4 do
            let mutable unmarked = false
            for x = 0 to 4 do 
                if not (Set.contains (board[y][x]) marked) then
                    unmarked <- true
            if not unmarked then
                victory <- true
        for x = 0 to 4 do 
            let mutable unmarked = false
            for y = 0 to 4 do    
                if not (Set.contains (board[y][x]) marked) then
                    unmarked <- true
            if not unmarked then
                victory <- true
        victory

    let unmarked marked board = 
        board |> Array.collect id |> Array.sumBy (fun s -> if Set.contains s marked then 0 else int s)

    boards 
    |> List.map (fun b ->
        let rec count soFar numbers =
            match numbers with
            | next::rem ->
                let nextMarked = Set.add next soFar
                if wins nextMarked b then
                    Set.count nextMarked, (unmarked nextMarked b) * int next
                else
                    count nextMarked rem
            | _ -> failwith "shouldn't be reached"
        count Set.empty numbers)
    |> List.maxBy (fst)
    |> snd
