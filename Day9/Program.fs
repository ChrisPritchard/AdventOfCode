open System
open System.IO
open FParsec.CharParsers
open FParsec

let part1 _ = 0
let part2 _ = 0

let place newMarble (marbles, currentMarbleIndex) =
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

[<EntryPoint>]
let main _ =

    let input = File.ReadAllText "input.txt"
    let players, maxMarble = 
        match run (pint32 .>> pstring " players; last marble is worth " .>>. pint32 .>> pstring " points") input with
        | Success (r, _, _) -> r
        | _ -> failwith "invalid input"

    let board = [0..22] |> List.fold (fun board marble -> place marble board) ([], 0)
    printfn "%A %i" (fst board) (snd board)

    printfn "part 1: %i" <| part1 0
    printfn "part 2: %i" <| part2 0

    0