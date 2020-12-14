module Day14

open Common
open System
open System.IO

let input = File.ReadAllLines "./inputs/day14.txt"

let processed () =
    input |> List.ofArray

let rec asBinary (value: int64) = Convert.ToString (value, 2)

let part1 () =
    let rec processor memory mask (instructions: string list) =
        match instructions with
        | [] ->
            Map.toArray memory |> Array.sumBy snd
        | s::rest when s.StartsWith "mask" ->
            processor memory (s.["mask = ".Length..] |> Seq.toArray) rest
        | s::rest ->
            let parts = s |> splits [|"mem[";"] = "|] |> Array.map int64
            let binary = 
                ((asBinary parts.[1]).PadLeft mask.Length).Replace(" ", "0")
                |> Seq.mapi (fun i c -> if mask.[i] <> 'X' then mask.[i] else c)
                |> asString
            let value = Convert.ToInt64 (binary, 2)
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
            let parts = s |> splits [|"mem[";"] = "|] |> Array.map int64
            let binary = 
                ((asBinary parts.[0]).PadLeft mask.Length).Replace(" ", "0")
                |> Seq.toArray
                |> Array.mapi (fun i c -> if mask.[i] = '0' then c else mask.[i])
            let rec updater (acc: string) i map =
                if i >= binary.Length then
                    let address = Convert.ToInt64 (acc, 2)
                    Map.add address parts.[1] map
                else
                    let c = binary.[i]
                    if c = 'X' then
                        let map = updater (acc + "0") (i+1) map
                        updater (acc + "1") (i+1) map
                    else
                        updater (acc + string c) (i+1) map
            processor (updater "" 0 memory) mask rest
    processor Map.empty Array.empty (processed ())