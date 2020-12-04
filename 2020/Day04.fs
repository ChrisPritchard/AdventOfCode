module Day04

open System
open System.IO
open Common

let input = 
    (File.ReadAllText ("./inputs/day04.txt")).Split([|"\r\n\r\n"|], StringSplitOptions.RemoveEmptyEntries)
    |> Array.map (fun p -> p |> split "\r\n: " |> Array.chunkBySize 2 |> Array.map (fun a -> a.[0], a.[1]) |> Map.ofArray)
        
let required = [|"byr"; "iyr"; "eyr"; "hgt"; "hcl"; "ecl"; "pid"|]
let optional = "cid"

let part1 () =
    input 
    |> Array.filter (fun p -> required |> Array.forall (fun k -> Map.containsKey k p))
    |> Array.length

let tryParseInt (s: string) = 
    try 
        s |> int |> Some
    with :? FormatException -> 
        None


let rules = 
    [|
        "byr", fun (v : string) -> 
            match tryParseInt v with
            | Some v -> v >= 1920 && v <= 2002
            | None -> false
        "iyr", fun (v : string) -> 
            match tryParseInt v with
            | Some v -> v >= 2010 && v <= 2020
            | None -> false
        "eyr", fun (v : string) -> 
            match tryParseInt v with
            | Some v -> v >= 2020 && int v <= 2030
            | None -> false
        "hgt", fun (v : string) -> 
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
        "hcl", fun (v : string) -> 
            v.StartsWith("#") && v.Length = 7 && (split "abcdef0123456789" v.[1..]).Length = 0
        "ecl", fun (v: string) ->
            Array.contains v [|"amb"; "blu"; "brn"; "gry"; "grn"; "hzl"; "oth"|]
        "pid", fun (v : string) -> 
            v.Length = 9 && (split "0123456789" v).Length = 0
    |]

let part2 () =
    input 
    |> Array.filter (fun p -> rules |> Array.forall (fun (k, r) -> 
            if not (Map.containsKey k p) then false
            else 
                let v = r p.[k]
                if not v then printfn "%s with value %s failed" k p.[k]
                v))
    |> Array.length
