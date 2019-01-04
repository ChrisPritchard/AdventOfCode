
open System.IO
open FParsec.Primitives
open FParsec.CharParsers

[<EntryPoint>]
let main _ =

    let input = File.ReadAllText "input.txt"

    let pNumberSum = many (pint32 <|> (anyChar |>> fun _ -> 0)) |>> List.sum

    let part1 = match run pNumberSum input with Success (r, _, _) -> r | Failure (ex, _, _) -> failwith ex
    printfn "part 1: %i" part1

    0
