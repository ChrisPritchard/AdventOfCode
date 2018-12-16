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
            if List.tryFind snd segment = None then fst segment.[2], false
            else
                let key = List.map snd segment 
                match Map.tryFind key rules with
                | Some n -> fst segment.[2], n
                | _ -> fst segment.[2], false)
    state.[0..1] @ next @ state.[state.Length - 2..]

let sumLine = List.sumBy (fun (i, b) -> if b then i else 0L)

[<EntryPoint>]
let main _ =
    
    let input = File.ReadAllLines "input.txt"
    let initialState, rules = parseInput input

    let part1 = 
        let paddedState = padState initialState 20L
        let final = [1..20] |> List.fold (fun state _ -> advance state rules) paddedState
        sumLine final
    
    printfn "part1 sum: %i " part1

    let rec findStable lastLine iterations lastIncrement =
        let lastSum = sumLine lastLine
        let next = advance lastLine rules
        let sum = sumLine next
        if sum - lastSum = lastIncrement then lastSum, iterations, lastIncrement
        else
            findStable next (iterations + 1L) (sum - lastSum)

    let paddedState = padState initialState 100L
    let baseSum, iterations, increment = findStable paddedState 0L 0L
    let part2 = baseSum + (increment * (50000000000L - iterations))

    printfn "part 2 sum: %i" part2

    0
