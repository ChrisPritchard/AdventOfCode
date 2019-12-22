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

    //([|0..9|], input) ||> Array.fold apply

let part2 () =
    
    let reverseIncrement totalLength index increment =
        if index = 0L then 0L
        else (index * -increment) % totalLength + totalLength

    let reverseCut totalLength index cut =
        let cut = -cut
        if cut >= 0L then
            if cut <= index then index - cut
            else totalLength - (cut - index)
        else
            let point = totalLength + cut
            if point <= index then index - point
            else abs cut + index

    let originalIndex totalLength index expression =
        match expression with
        | "deal into new stack" -> totalLength - 1L - index
        | s when s.StartsWith "deal with increment " ->
            let increment = s.Substring "deal with increment ".Length |> int64
            reverseIncrement totalLength index increment
        | s when s.StartsWith "cut " ->
            let cut = s.Substring "cut ".Length |> int64
            reverseCut totalLength index cut
        | _ -> failwithf "'%s' didn't match a handler" expression
    
    let totalCards = 119315717514047L
    let iterations = 101741582076661L

    //(2019L, input) ||> Array.fold (apply 10007L)
    (4684L, Array.rev input) ||> Array.fold (originalIndex 10007L)

    //[|0; 7; 4; 1; 8; 5; 2; 9; 6; 3|]
    //|> Array.mapi (fun i _ -> applyIncrement 3L (int64 i) 10L true)

    //[|3; 4; 5; 6; 7; 8; 9; 0; 1; 2|]
    //|> Array.mapi (fun i _ -> reverseCut 10L (int64 i) 3L)

    //[|6; 7; 8; 9; 0; 1; 2; 3; 4; 5|]
    //|> Array.mapi (fun i _ -> reverseCut 10L (int64 i) -4L)

    //let test = [|
    //    "deal into new stack"
    //    "cut -2"
    //    "deal with increment 7"
    //    "cut 8"
    //    "cut -4"
    //    "deal with increment 7"
    //    "cut 3"
    //    "deal with increment 9"
    //    "deal with increment 3"
    //    "cut -1"
    //    |]
    //let orders = Array.init 10 (fun i -> 
    //    (int64 i, test) ||> Array.fold (apply 10L))
    //Array.init 10 (fun i -> Array.findIndex ((=) (int64 i)) orders)