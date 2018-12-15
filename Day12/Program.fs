open System
open System.IO
open FParsec.CharParsers
open FParsec

[<EntryPoint>]
let main _ =
    
    let input = File.ReadAllLines "input.txt"
    let initialState = 
        match run (pstring "initial state: " >>. many anyChar) input.[0] with 
        | Success (r, _, _) -> r |> List.mapi (fun i c -> i, c = '#')
        | _ -> failwith "invalid line"

    let paddedState = 
        ([-5..-1] |> List.map (fun i -> i, false))
        @
        initialState
        @
        ([1..5] |> List.map (fun i -> List.length initialState + i, false))

    let lineParser line = 
        match run ((parray 5 anyChar .>> pstring " => ") .>>. anyChar) line with
        | Success ((pattern, result), _, _) -> 
            pattern |> Array.toList |> List.map ((=) '#'), result = '#'
        | _ -> failwith "invalid line"

    let rules = input.[2..] |> Array.map lineParser |> Map.ofArray

    let advance state rules =
        let next = 
            state 
            |> List.windowed 5 
            |> List.map (fun segment ->
                let key = List.map snd segment 
                match Map.tryFind key rules with
                | Some n -> fst segment.[2], n
                | _ -> segment.[2])
        state.[0..1] @ next @ state.[state.Length - 2..]

    printfn "%A" initialState
    
    0
