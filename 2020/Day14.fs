module Day14

open Common
open System
open System.IO

let input = File.ReadAllLines "./inputs/day14.txt"

let processed () =
    input |> List.ofArray

let rec asBinary value =
  if value < 2UL then
    string value
  else
    let divisor = value/2UL
    let remainder = string (value % 2UL)
    asBinary divisor + remainder

let part1 () =
    let rec processor memory mask (instructions: string list) =
        match instructions with
        | [] ->
            Map.toArray memory |> Array.sumBy snd
        | s::rest when s.StartsWith "mask" ->
            processor memory (s.["mask = ".Length..] |> Seq.toArray) rest
        | s::rest ->
            let parts = s |> splits [|"mem[";"] = "|] |> Array.map uint64
            let binary = 
                ((asBinary parts.[1]).PadLeft mask.Length).Replace(" ", "0")
                |> Seq.mapi (fun i c -> if mask.[i] <> 'X' then mask.[i] else c)
                |> asString
            let value = Convert.ToUInt64 (binary.TrimStart([|'0'|]), 2)
            processor (Map.add parts.[0] value memory) mask rest
    processor Map.empty Array.empty (processed ())

let part2 () =
    let rec processor memory mask (instructions: string list) =
        match instructions with
        | [] ->
            Map.toArray memory |> Array.sumBy snd
        | s::rest when s.StartsWith "mask" ->
            processor memory (s.["mask = ".Length..] |> Seq.toArray) rest
        | s::rest ->
            let parts = s |> splits [|"mem[";"] = "|] |> Array.map uint64
            let binary = 
                ((asBinary parts.[0]).PadLeft mask.Length).Replace(" ", "0")
                |> Seq.toArray
                |> Array.mapi (fun i c -> if mask.[i] = '0' then c else mask.[i])
            let rec addresses (acc: string) i =
                if i >= binary.Length then
                    [|
                        Convert.ToUInt64 (acc.TrimStart([|'0'|]), 2)
                    |]
                else
                    let c = binary.[i]
                    if c = 'X' then
                        Array.collect id [|
                            addresses (acc + "0") (i+1)
                            addresses (acc + "1") (i+1)
                        |]
                    else
                        addresses (acc + string c) (i+1)
            let allAddresses = addresses "" 0
            let memory = (memory, allAddresses) ||> Array.fold (fun map address -> Map.add address parts.[1] map)
            processor memory mask rest
    processor Map.empty Array.empty (processed ())