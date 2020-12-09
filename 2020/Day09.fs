module Day09

open System.IO
open Common

let input = File.ReadAllLines "./inputs/day09.txt"

let processed () =
    input |> Array.map bigint.Parse

let part1 () =
    let processed = processed ()
    let check n preamble =
        let source = processed.[n-preamble-1..n-1] |> Array.indexed
        let valid = 
            source
            |> Array.collect (fun (i, n) ->
                source 
                |> Array.except ([|i, n|]) 
                |> Array.map (fun (_, n2) -> n + n2))
            |> Array.distinct
        Array.contains processed.[n] valid

    let preamble = 25
    [|preamble..Array.length processed|]
    |> Array.pick (fun index ->
        if check index preamble then None else Some processed.[index])

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