module Day05

open Common
open System

type State =
    | Reading of string list
    | Stacks of (char list) []

let createStacks (lines: string list) = 
    let bottom = List.head lines
    let stacks = Array.create ((bottom.Length + 1) / 4) List.empty
    (stacks, lines)
    ||> List.fold (fun stacks line ->
        stacks |> Array.mapi (fun i s ->
            let index = (i * 4) + 1
            if index >= line.Length then s 
            else if line[index] = ' ' then s
            else line[index]::s))
    |> Stacks

let crateMover shouldReverse = 
    (Reading [], readEmbeddedRaw "day05")
    ||> Seq.fold (fun state line ->
        match state with
        | Reading acc ->
            if line = "" then
                createStacks acc
            else if not (Char.IsNumber(line[1])) then
                Reading (line::acc)
            else
                state
        | Stacks stacks ->
            let orders = line |> split "movefromto " |> Array.map int
            let from, toStack, amt = orders[1] - 1, orders[2] - 1, orders[0]
            let toMove = stacks[from] |> List.take amt
            let toMove = if shouldReverse then List.rev toMove else toMove
            stacks 
            |> Array.mapi (fun i s -> if i = from then List.skip amt s else if i = toStack then List.append toMove s else s)
            |> Stacks
    )
    |> function | Stacks s -> Array.map List.head s | _ -> failwith "error"
    |> asString

let part1 () =
    crateMover true

let part2 () =
    crateMover false
