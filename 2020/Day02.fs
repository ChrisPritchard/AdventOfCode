module Day02

open System.IO
open System.Collections

let input = File.ReadAllLines ("./inputs/day02.txt") |> Array.map int

let part1 () =
    input 
    |> Array.pick (fun a ->
        input |> Array.tryPick (fun b -> if a + b = 2020 then Some (a * b) else None))

let part2 () =
    0