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

// in the below swapping the immutable map for a mutable dictionary improves runtime from 60s to about 2s
// startomg the dictionary with an appropriate capacity (~4 mil) shaves off 400-600 ms
// using an array big enough of o(1) lookups by index (~30 mil) drops it to ~600 ms

let part2 () = 
    let nums = processed ()
    let acc = Array.create 30000000 0 // max number here - who cares about memory usage? :D
    for (k, v) in nums |> Array.take (nums.Length - 1) |> Array.mapi (fun i n -> n, (i+1)) do
        acc.[k] <- v
    let rec memory i last =
        if last >= acc.Length then 
            printfn "%A" last
        let next = if acc.[last] <> 0 then i - 1 - acc.[last] else 0
        acc.[last] <- i - 1
        if i = 30000000 then next else memory (i + 1) next
    memory (nums.Length + 1) (nums.[nums.Length - 1])