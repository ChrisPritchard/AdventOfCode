module Day15

open Common
open System.IO
open System.Collections.Generic

let input = File.ReadAllLines "./inputs/day15.txt"

let processed () = split "," input.[0] |> Array.map int

let part1 () = 
    let nums = processed ()
    let acc = nums |> Array.take (nums.Length - 1) |> Array.mapi (fun i n -> n, (i+1)) |> Map.ofArray
    let rec memory i last acc =
        let next = if Map.containsKey last acc then i - 1 - acc.[last] else 0
        if i = 2020 then next else memory (i + 1) next (Map.add last (i - 1) acc)
    memory (nums.Length + 1) (nums.[nums.Length - 1]) acc

// the below swaps the immutable map for a mutable dictionary, which improves runtime from 60s to about 2s

let part2 () = 
    let nums = processed ()
    let acc = Dictionary<int, int>()
    for (k, v) in nums |> Array.take (nums.Length - 1) |> Array.mapi (fun i n -> n, (i+1)) do
        acc.Add(k, v)
    let rec memory i last =
        let next = if acc.ContainsKey last then i - 1 - acc.[last] else 0
        acc.[last] <- i - 1
        if i = 30000000 then next else memory (i + 1) next
    memory (nums.Length + 1) (nums.[nums.Length - 1])