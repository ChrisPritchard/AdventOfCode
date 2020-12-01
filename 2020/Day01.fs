module Day01

open System.IO
open System.Collections

let input = File.ReadAllLines ("./inputs/day01.txt") |> Array.map int

let part1 () =
    input 
    |> Array.pick (fun a ->
        input |> Array.tryPick (fun b -> if a + b = 2020 then Some (a * b) else None))

let part2 () =
    input 
    |> Array.pick (fun a ->
        input |> Array.tryPick (fun b ->
            if a + b >= 2020 then None
            else
                input |> Array.tryPick (fun c -> if a + b + c = 2020 then Some (a * b * c) else None)))