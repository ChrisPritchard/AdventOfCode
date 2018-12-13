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
        let adjusted = if nextIndex >= n then nextIndex - n else nextIndex
        (List.take (adjusted - 1) marbles) @ [newMarble] @ (List.skip (adjusted + 1) marbles), nextIndex

[<EntryPoint>]
let main _ =

    let input = File.ReadAllText "input.txt"
    let players, maxMarble = 
        match run (pint32 .>> pstring " players; last marble is worth " .>>. pint32 .>> pstring " points") input with
        | Success (r, _, _) -> r
        | _ -> failwith "invalid input"

    let board = [0..5] |> List.fold (fun board marble -> place marble board) ([], 0)

    printfn "part 1: %i" <| part1 0
    printfn "part 2: %i" <| part2 0

    0