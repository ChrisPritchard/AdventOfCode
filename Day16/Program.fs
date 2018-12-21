open System
open System.IO
open FParsec.CharParsers
open FParsec

type Input = 
    | Example of instruction: int list * before: int list * after: int list
    | Input of int list

let parseInput input = 
    let pRegState = sepBy pint32 (pstring ", ") .>> pstring "]"
    let pBefore = pstring "Before: [" >>. pRegState
    let pAfter = pstring "After:  [" >>. pRegState
    let pInstruction = parray 4 (pint32 .>> opt (pchar ' ')) .>> newline |>> Array.toList
    let pExample = (pBefore .>> newline) .>>. pInstruction .>>. pAfter |>> fun ((b, i), a) -> Example (i, b, a)
    let pInput = many ((pExample .>> spaces) <|> (pInstruction |>> Input))

    let parsed = 
        match run pInput input with
        | Success (r, _, _) -> r
        | Failure (e, _, _) -> failwith e

    parsed 
    |> List.fold (fun (es, is) -> 
        function 
        | Example (i, b, a) -> (i, b, a)::es, is 
        | Input list -> es, list::is) ([], [])
    |> fun (e, i) -> e, List.rev i

[<EntryPoint>]
let main _ =
    
    let input = File.ReadAllText "input.txt"
    let examples, instructions = parseInput input
    
    0
