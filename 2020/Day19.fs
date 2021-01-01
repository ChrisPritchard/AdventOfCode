module Day19

open Common
open System.IO

let input = File.ReadAllText "./inputs/day19.txt"

type Rule =
    | Rule of int
    | Char of char

let processLine s =
    let parts = s |> splitOn ": "
    let options = 
        parts.[1] |> splitOn " | " |> Array.map (fun opt -> 
            opt |> splitOn " " |> Array.map (fun s -> 
                if s.[0] = '"' then Char s.[1] else Rule (int s)))
    int parts.[0], options

let processed () = 
    let newline = if input.Contains("\r\n") then "\r\n" else "\n"
    let parts = input |> splitOn (newline+newline)
    let rules = 
        parts.[0] 
        |> splitOn newline 
        |> Array.map processLine
        |> Map.ofArray
    let samples = parts.[1] |> splitOn newline
    rules, samples

let test s rules =
    let start = Map.find 0 rules
    let rec valid (ss: string) rule =
        rule 
        |> Array.collect (fun opt ->
            ([|ss|], opt)
            ||> Array.fold (fun acc r ->
                match r with
                | Char c -> 
                    acc |> Array.choose (fun s -> if s.Length <> 0 && s.[0] = c then Some s.[1..] else None)
                | Rule r ->
                    acc |> Array.collect (fun s -> valid s rules.[r])))
    valid s start |> Array.contains ""

let part1 () =
    let rules, samples = processed ()
    samples |> Array.filter (fun s -> test s rules) |> Array.length

let part2 () = 
    let rules, samples = processed ()
    let toAdd = [|
        processLine "8: 42 | 42 8"
        processLine "11: 42 31 | 42 11 31"
    |]
    let rules = (rules, toAdd) ||> Array.fold (fun rules (index, options) -> Map.add index options rules)
    samples |> Array.filter (fun s -> test s rules) |> Array.length