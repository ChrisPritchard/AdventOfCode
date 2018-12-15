open System
open System.IO
open FParsec.CharParsers
open FParsec

[<EntryPoint>]
let main _ =
    
    let input = File.ReadAllLines "input.txt"
    let initialState = 
        match run (pstring "initial state: " >>. many anyChar) input.[0] with 
        | Success (r, _, _) -> r |> List.map ((=) '#')
        | _ -> failwith "invalid line"

    let lineParser line = 
        match run ((parray 5 anyChar .>> pstring " => ") .>>. anyChar) line with
        | Success ((pattern, result), _, _) -> 
            pattern |> Array.toList |> List.map ((=) '#'), result = '#'
        | _ -> failwith "invalid line"

    let rules = input.[2..] |> Array.map lineParser |> Map.ofArray

    printfn "%A" initialState
    
    0
