module Day09

open System.IO
open Common

let input = File.ReadAllLines "./inputs/day09.txt"

let processed () =
    input |> Array.map bigint.Parse

let part1 () =
    let processed = processed ()
    let preamble = 25
    let rec finder soFar l i =
        let n = processed.[i]
        if i < preamble then
            finder (Set.add n soFar) (n::l) (i + 1)
        else
            if List.forall (fun o -> not (Set.contains (n - o) soFar)) l then
                n
            else
                finder (Set.add n soFar) (n::l) (i + 1)
    finder Set.empty [] 0

let part2 () =
    let target = part1 ()
    let processed = processed ()
    let max = Array.length processed
    [|0..max|]
    |> Array.pick (fun index ->
        [|index+1..max|]
        |> Array.tryPick (fun i -> 
            let range = processed.[index..i]
            if Array.sum range = target then Some (Array.min range + Array.max range) else None)
    )