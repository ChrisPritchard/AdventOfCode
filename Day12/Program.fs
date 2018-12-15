open System
open System.IO
open FParsec.CharParsers
open FParsec

let parseInput (lines : string []) = 
    let initialState = 
        match run (pstring "initial state: " >>. many anyChar) lines.[0] with 
        | Success (r, _, _) -> r |> List.mapi (fun i c -> int64 i, c = '#')
        | _ -> failwith "invalid line"

    let lineParser line = 
        match run ((parray 5 anyChar .>> pstring " => ") .>>. anyChar) line with
        | Success ((pattern, result), _, _) -> 
            pattern |> Array.toList |> List.map ((=) '#'), result = '#'
        | _ -> failwith "invalid line"

    initialState, lines.[2..] |> Array.map lineParser |> Map.ofArray

let padState state iterations = 
    let length = List.length state |> int64
    ([ for n = -iterations to -1L do yield n ] |> List.map (fun i -> i, false))
    @
    state
    @
    ([ for n = 0L to iterations - 1L do yield n ] |> List.map (fun i -> length + i, false))

let advance state rules =
    let next = 
        state 
        |> List.windowed 5 
        |> List.map (fun segment ->
            let key = List.map snd segment 
            match Map.tryFind key rules with
            | Some n -> fst segment.[2], n
            | _ -> fst segment.[2], false)
    state.[0..1] @ next @ state.[state.Length - 2..]

[<EntryPoint>]
let main _ =
    
    let input = File.ReadAllLines "input.txt"
    let initialState, rules = parseInput input

    let part1 = 
        let paddedState = padState initialState 20L
        let final = [1..20] |> List.fold (fun state _ -> advance state rules) paddedState
        List.sumBy (fun (i, b) -> if b then i else 0L) final
    
    printfn "sum: %i " part1

    0
