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

    let length = List.length initialState
    let paddedState = 
        ([-20..-1] |> List.map (fun i -> i, false))
        @
        initialState
        @
        ([0..19] |> List.map (fun i -> length + i, false))

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
                | _ -> fst segment.[2], false)
        state.[0..1] @ next @ state.[state.Length - 2..]

    let printline = List.map (fun (_,b) -> if b then '#' else '.') >> System.String.Concat >> printfn "%s"

    printline paddedState
    let results, final = 
        [1..20] |> List.mapFold (fun state _ -> 
        let result = advance state rules
        result, result) paddedState
    List.iter printline results
    
    printfn "sum: %i " <| List.sumBy (fun (i, b) -> if b then i else 0) final

    0
