open System
open System.IO

let part1 input =
    0

[<EntryPoint>]
let main _ =

    let input = (File.ReadAllText "input.txt").Split([|' '|])
    printfn "part 1: %i" <| part1 input

    0