open System
open System.IO
open FParsec.CharParsers
open FParsec

let part1 _ = 0
let part2 _ = 0

[<EntryPoint>]
let main _ =

    let input = File.ReadAllText "input.txt"
    let players, maxMarble = 
        match run (pint32 .>> pstring " players; last marble is worth " .>>. pint32 .>> pstring " points") input with
        | Success (r, _, _) -> r
        | _ -> failwith "invalid input"

    printfn "part 1: %i" <| part1 0
    printfn "part 2: %i" <| part2 0

    0