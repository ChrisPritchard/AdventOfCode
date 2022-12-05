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

let part1 () =
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
            state
    )


let part2 () =
    readEmbeddedRaw "day05"


