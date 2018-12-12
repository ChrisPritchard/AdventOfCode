open System
open System.IO
open FParsec.CharParsers
open FParsec

let rec part1 (finished : char list) rules =
    function
    | [] -> String.Concat (List.rev finished)
    | remaining ->
        let next = remaining |> Seq.find (fun char ->
            rules 
            |> List.filter (fun (pre, c) -> c = char && not (Seq.contains pre finished)) 
            |> List.map fst
            |> List.isEmpty)
        part1 (next::finished) rules (remaining |> List.filter (fun char -> char <> next))

let rec part2 (finished : char list) working idle rules =
    function
    | [] -> String.Concat (List.rev finished)
    | remaining ->
        let newWorking = working |> List.map (fun i -> i - 1) |> List.except [0]
        let newIdle = idle + (List.length working - List.length newWorking)
        if newIdle = 0 then
            part2 finished newWorking newIdle rules remaining
        else
            
    0

[<EntryPoint>]
let main _ =

    let ppre = pstring "Step " >>. anyChar
    let psub = pstring " must be finished before step " >>. anyChar .>> pstring " can begin."
    let pline = ppre .>>. psub
    let processLine line =
        match run pline line with
        | Success (result, _, _) -> result
        | Failure (error, _, _) -> failwith error
        
    let lines = File.ReadAllLines "input.txt"
    let rules = lines |> Seq.map processLine |> Seq.toList

    printfn "part 1: %s" <| part1 [] rules ['A'..'Z']
    printfn "part 2: %i" <| part2 [] [] 5 rules ['A'..'Z']

    0