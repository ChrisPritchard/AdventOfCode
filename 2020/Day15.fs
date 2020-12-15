module Day15

open Common
open System.IO

let input = File.ReadAllLines "./inputs/day15.txt"

let processed () = split "," input.[0] |> Array.map int

let part1 () = 
    let nums = processed ()
    let acc = nums |> Array.take (nums.Length - 1) |> Array.mapi (fun i n -> n, (i+1)) |> Map.ofArray
    let rec memory i last acc =
        let next = if Map.containsKey last acc then i - 1 - acc.[last] else 0
        if i = 2020 then next else memory (i + 1) next (Map.add last (i - 1) acc)
    memory (nums.Length + 1) (nums.[nums.Length - 1]) acc

// the below solves this in 60 seconds, so needs some optimisation

let part2 () = 
    let nums = processed ()
    let acc = nums |> Array.take (nums.Length - 1) |> Array.mapi (fun i n -> n, (i+1)) |> Map.ofArray
    let rec memory i last acc =
        let next = if Map.containsKey last acc then i - 1 - acc.[last] else 0
        if i = 30000000 then next else memory (i + 1) next (Map.add last (i - 1) acc)
    memory (nums.Length + 1) (nums.[nums.Length - 1]) acc