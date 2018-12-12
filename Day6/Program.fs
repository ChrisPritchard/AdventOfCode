open System.IO
open FParsec.CharParsers
open FParsec

[<EntryPoint>]
let main _ =
    
    let lines = File.ReadAllLines "input.txt"
    let lines = [|
        "1, 1"
        "1, 6"
        "8, 3"
        "3, 4"
        "5, 5"
        "8, 9"
    |]

    let pline = pint32 .>> pstring ", " .>>. pint32
    let processLine line =
        match run pline line with
        | Success (result, _, _) -> result
        | Failure (error, _, _) -> failwith error

    let points = lines |> Seq.map processLine |> Seq.toList

    0