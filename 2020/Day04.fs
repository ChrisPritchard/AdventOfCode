module Day04

open System
open System.IO
open Common

let input = 
    (File.ReadAllText ("./inputs/day04.txt"))
    |> splitOn "\r\n\r\n"
    |> Array.map (fun p -> 
        split "\r\n: " p 
        |> Array.chunkBySize 2 
        |> Array.map (fun a -> a.[0], a.[1]) 
        |> Map.ofArray)
        
let required = [|"byr"; "iyr"; "eyr"; "hgt"; "hcl"; "ecl"; "pid"|]

let part1 () =
    input 
    |> Array.filter (fun passport -> 
        required 
        |> Array.forall (fun key -> 
            Map.containsKey key passport))
    |> Array.length

let tryParseInt (s: string) = 
    try 
        s |> int |> Some
    with :? FormatException -> 
        None

let rules = 
    [|
        "byr", fun v -> 
            match tryParseInt v with
            | Some v -> v >= 1920 && v <= 2002
            | None -> false
        "iyr", fun v -> 
            match tryParseInt v with
            | Some v -> v >= 2010 && v <= 2020
            | None -> false
        "eyr", fun v -> 
            match tryParseInt v with
            | Some v -> v >= 2020 && int v <= 2030
            | None -> false
        "hgt", fun v -> 
            if v.EndsWith("cm") then
                let v = v.Replace("cm", "")
                match tryParseInt v with
                | Some v -> v >= 150 && int v <= 193
                | None -> false
            elif v.EndsWith("in") then
                let v = v.Replace("in", "")
                match tryParseInt v with
                | Some v -> v >= 59 && int v <= 76
                | None -> false
            else
                false
        "hcl", fun v -> 
            v.StartsWith("#") && v.Length = 7 && (split "abcdef0123456789" v.[1..]).Length = 0
        "ecl", fun v ->
            Array.contains v [|"amb"; "blu"; "brn"; "gry"; "grn"; "hzl"; "oth"|]
        "pid", fun v -> 
            v.Length = 9 && (split "0123456789" v).Length = 0
    |]

let part2 () =
    input 
    |> Array.filter (fun passport -> 
        rules 
        |> Array.forall (fun (key, rule) -> 
            Map.containsKey key passport && rule passport.[key]))
    |> Array.length
