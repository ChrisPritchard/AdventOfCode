module Day08

open System.IO
open Common

let input = File.ReadAllLines "./inputs/day08.txt"

let processed () =
    input |> Array.map (fun l -> split " +" l |> fun s -> s.[0], int s.[1])

let part1 () =
    let instructions = processed ()
    let rec processor ip acc visited =
        if Set.contains ip visited then
            acc
        else
            let visited = Set.add ip visited
            let acc, ip = 
                match instructions.[ip] with
                | "acc", n -> acc + n, ip + 1
                | "jmp", n -> acc, ip + n
                | _ -> acc, ip + 1
            processor ip acc visited
    processor 0 0 Set.empty
 
let part2 () =
    let instructions = processed ()
    let rec processor ip acc visited faulty =
        if Set.contains ip visited then
            None
        elif ip = Array.length instructions then
            Some acc
        else
            let visited = Set.add ip visited
            match instructions.[ip] with
            | "acc", n ->
                processor (ip + 1) (acc + n) visited faulty
            | "jmp", n when not faulty ->
                processor (ip + n) acc visited false
            | "nop", _ when not faulty -> 
                processor (ip + 1) acc visited false
            | "jmp", n ->
                processor (ip + n) acc visited true
                |> Option.orElse (processor (ip + 1) acc visited false)
            | _, n -> 
                processor (ip + 1) acc visited true
                |> Option.orElse (processor (ip + n) acc visited false)
    processor 0 0 Set.empty true |> Option.get
    