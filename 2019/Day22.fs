module Day22

open Common
open System
open System.IO
open System.Collections.Generic

let input = File.ReadAllLines "./inputs/day22.txt"

let part1 () =

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

    let res = ([|0..10006|], input) ||> Array.fold apply
    Array.findIndex ((=) 2019) res

let part2 () =
    
    let applyIncrement increment index totalLength =
        if index = 0L then 0L
        else (index * increment) % totalLength
    
    let applyCut cut index totalLength =
        if cut >= 0L then
            if cut <= index then index - cut
            else totalLength - (cut - index)
        else
            let point = totalLength + cut
            if point <= index then index - point
            else abs cut + index
    
    let apply totalLength index =
        function
        | "deal into new stack" -> totalLength - 1L - index
        | s when s.StartsWith "deal with increment " ->
            let increment = s.Substring "deal with increment ".Length |> int64
            applyIncrement increment index totalLength
        | s when s.StartsWith "cut " ->
            let cut = s.Substring "cut ".Length |> int64
            applyCut cut index totalLength
        | s -> failwithf "'%s' didn't match a handler" s
    
    let totalCards = 119315717514047L
    let iterations = 101741582076661L

    let applyIteration index = 
        (index, input) ||> Array.fold (apply totalCards)

    let rec processor cnt index =
        let result = applyIteration index
        if result = 0L then cnt
        else
            processor (cnt + 1) result

    processor 0 0L