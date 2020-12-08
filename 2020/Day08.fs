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
    processed ()