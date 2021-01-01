module Day04

open System
open System.IO
open Common

let input = File.ReadAllText "./inputs/day04.txt"
        
let processed () =
    let newline = if input.Contains("\r\n") then "\r\n" else "\n"
    input
    |> splitOn (newline + newline)
    |> Array.map (fun p -> 
        split "\r\n: " p 
        |> Array.chunkBySize 2 
        |> Array.map (fun a -> a.[0], a.[1]) 
        |> Map.ofArray)

let part1 () =
    let required = [|"byr"; "iyr"; "eyr"; "hgt"; "hcl"; "ecl"; "pid"|]
    processed () 
    |> Array.filter (fun passport -> 
        required 
        |> Array.forall (fun key -> 
            Map.containsKey key passport))
    |> Array.length

let part2 () =

    let tryParseInt (s: string) = 
        try 
            s |> int |> Some
        with :? FormatException -> 
            None

    let inRange min max v =
        match tryParseInt v with
        | Some v -> v >= min && v <= max
        | None -> false

    let rules = 
        [|
            "byr", inRange 1920 2002
            "iyr", inRange 2010 2020
            "eyr", inRange 2020 2030
            "hgt", fun v -> 
                if v.EndsWith("cm") then
                    v.Replace("cm", "") |> inRange 150 193
                elif v.EndsWith("in") then
                    v.Replace("in", "") |> inRange 59 76
                else
                    false
            "hcl", fun v -> 
                v.StartsWith("#") && v.Length = 7 && (split "abcdef0123456789" v.[1..]).Length = 0
            "ecl", fun v ->
                Array.contains v [|"amb"; "blu"; "brn"; "gry"; "grn"; "hzl"; "oth"|]
            "pid", fun v -> 
                v.Length = 9 && (split "0123456789" v).Length = 0
        |]

    processed ()
    |> Array.filter (fun passport -> 
        rules 
        |> Array.forall (fun (key, rule) -> 
            Map.containsKey key passport && rule passport.[key]))
    |> Array.length
