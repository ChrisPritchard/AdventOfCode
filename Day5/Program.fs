open System
open System.IO

let rec part1 polymer =
    Seq.length polymer

[<EntryPoint>]
let main _ =
    
    //let polymer = File.ReadAllText "input.txt"
    let polymer = "dabAcCaCBAcCcaDA"

    printfn "part1: %i" <| part1 polymer

    0