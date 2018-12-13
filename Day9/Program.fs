open System
open System.IO
open FParsec.CharParsers
open FParsec

let placeMarble newMarble (marbles, currentMarbleIndex) =
    match List.length marbles with
    | 0 -> [newMarble], 0
    | 1 -> marbles @ [newMarble], 1
    | 2 -> marbles.[0]::newMarble::[marbles.[1]], 1
    | n ->
        let nextIndex = currentMarbleIndex + 2
        if nextIndex = n then
            marbles @ [newMarble], nextIndex
        else if nextIndex = n + 1 then
            marbles.[0]::newMarble::marbles.[1..], 1
        else
            marbles.[0..nextIndex-1] @ [newMarble] @ marbles.[nextIndex..], nextIndex

let scoreMarble (marbles, currentMarbleIndex) =
    let length = List.length marbles
    let toRemove = 
        if currentMarbleIndex < 7 then length - (7 - currentMarbleIndex)
        else currentMarbleIndex - 7
    (marbles.[0..toRemove-1] @ marbles.[toRemove+1..], toRemove), marbles.[toRemove]
     
let addScore player score scores = 
    let existing = match Map.tryFind player scores with Some n -> n | _ -> 0
    Map.add player (existing + score) scores


let part1 players maxMarble = 
    let _, finalScores =
        [1..maxMarble] |> List.fold (fun (board, scores) marble -> 
            let player = (marble - 1) % players
            if marble % 23 = 0 then
                let next, removed = scoreMarble board
                next, addScore player (removed + marble) scores
            else
                placeMarble marble board, scores
            ) (([0],0), Map.empty)
    finalScores |> Map.toList |> List.map snd |> List.max

let part2 _ = 0

[<EntryPoint>]
let main _ =

    let input = File.ReadAllText "input.txt"
    let players, maxMarble = 
        match run (pint32 .>> pstring " players; last marble is worth " .>>. pint32 .>> pstring " points") input with
        | Success (r, _, _) -> r
        | _ -> failwith "invalid input"

    printfn "part 1: %i" <| part1 players maxMarble
    printfn "part 2: %i" <| part2 0

    0