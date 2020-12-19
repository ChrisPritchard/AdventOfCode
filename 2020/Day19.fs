module Day19

open Common
open System.IO

let input = File.ReadAllText "./inputs/day19.txt"

type Rule =
    | Rule of int
    | Char of char

let processed () = 
    let parts = input |> splitOn (newline+newline)
    let rules = 
        parts.[0] 
        |> splitOn newline 
        |> Array.map (fun s ->
            let parts = s |> splitOn ": "
            let options = 
                parts.[1] |> splitOn " | " |> Array.map (fun opt -> 
                    opt |> splitOn " " |> Array.map (fun s -> 
                        if s.[0] = '"' then Char s.[1] else Rule (int s)))
            int parts.[0], options)
        |> Map.ofArray
    let samples = parts.[1] |> splitOn newline
    rules, samples

let part1 () =
    let rules, samples = processed ()
    let rec gather rule =
        rule |> Array.collect (fun opt ->
            ([|""|], opt)
            ||> Array.fold (fun acc r ->
                match r with
                | Char c -> 
                    acc |> Array.map (fun s -> s + string c)
                | Rule r -> 
                    let next = gather rules.[r]
                    acc |> Array.collect (fun o -> next |> Array.map (fun s -> o + s))))
    let candidates = 
        gather rules.[0]
        |> Set.ofArray
    samples |> Array.filter (fun s -> Set.contains s candidates) |> Array.length

let part2 () = 
    0