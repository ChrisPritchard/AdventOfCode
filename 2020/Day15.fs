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

// swapping the immutable map in the above for a mutable dictionary improves runtime from 60s to about 2s in part 2
// starting the dictionary with an appropriate capacity (~4 mil) drops it to 1-1.8 seconds
// using an array big enough (~30 mil, allowing for o(1) lookups and assignments) drops it to ~600 ms

let part2 () = 
    let nums = processed ()
    let acc = Array.create 30000000 0 // max number here - who cares about memory usage? :D
    for (k, v) in nums |> Array.take (nums.Length - 1) |> Array.mapi (fun i n -> n, (i+1)) do
        acc.[k] <- v
    let rec memory i last =
        let next = if acc.[last] <> 0 then i - 1 - acc.[last] else 0
        acc.[last] <- i - 1
        if i = 30000000 then next else memory (i + 1) next
    memory (nums.Length + 1) (nums.[nums.Length - 1])