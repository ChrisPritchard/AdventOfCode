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

    let lines = File.ReadAllLines "input.txt"

    printfn "part 1: %i" <| part1 lines
    printfn "part 2: %i" <| part2 lines

    0