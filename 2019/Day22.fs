module Day22

open Common
open System
open System.IO
open System.Collections.Generic

let input = File.ReadAllLines "./inputs/day22.txt"

let applyIncrement increment deck =
    let length = Array.length deck
    let result = Array.create length 0
    for i = 0 to length - 1 do
        if i = 0 then result.[0] <- deck.[0]
        else 
            let index = (i * increment) % length
            result.[index] <- deck.[i]
    result

let rec applyCut cut deck =
    if cut >= 0 then
        let taken = Array.take cut deck
        let start = Array.skip cut deck
        Array.append start taken
    else
        let point = Array.length deck + cut
        applyCut point deck

let apply deck =
    function
    | "deal into new stack" -> Array.rev deck
    | s when s.StartsWith "deal with increment " ->
        let increment = s.Substring "deal with increment ".Length |> int
        applyIncrement increment deck
    | s when s.StartsWith "cut " ->
        let cut = s.Substring "cut ".Length |> int
        applyCut cut deck
    | s -> failwithf "'%s' didn't match a handler" s

let part1 () =

    let res = ([|0..10006|], input) ||> Array.fold apply
    Array.findIndex ((=) 2019) res

    //([|0..9|], input) ||> Array.fold apply

let part2 () =

    0