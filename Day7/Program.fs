open System
open System.IO
open FParsec.CharParsers
open FParsec

let part1 lines =
    0

let part2 lines =
    0

[<EntryPoint>]
let main _ =

    let ppre = pstring "Step " >>. pchar
    let psub = pstring "must be finished before step " >>. pchar .>> pstring "can begin."
    let pline = ppre .>>. psub
    let processLine line =
        match run pline line with
        | Success (result, _, _) -> result
        | Failure (error, _, _) -> failwith error
        
    let lines = File.ReadAllLines "input.txt"

    printfn "part 1: %i" <| part1 lines
    printfn "part 2: %i" <| part2 lines

    0