open System
open System.IO
open FParsec.CharParsers
open FParsec

let nextAvailable rules finished remaining = 
    remaining 
    |> Seq.tryFind (fun char ->
        rules 
        |> List.filter (fun (pre, c) -> c = char && not (Seq.contains pre finished)) 
        |> List.map fst
        |> List.isEmpty)

let rec part1 (finished : char list) rules =
    function
    | [] -> String.Concat (List.rev finished)
    | remaining ->
        match nextAvailable rules finished remaining with
        | Some next ->
            part1 (next::finished) rules (remaining |> List.filter (fun char -> char <> next))
        | _ -> failwith "invalid input"

let processJobs working idle finished =
    working 
    |> List.fold (fun (nw, ni, nf) (char, time) ->
        match time with
        | 0 -> nw, ni + 1, char::nf
        | _ -> (char, time - 1)::nw, ni, nf) 
        ([], idle, finished)

let rec part2 (finished : char list) working idle baseJobTime rules remaining totalTime =
    let newTotalTime = totalTime + 1
    let newWorking, newIdle, newFinished = processJobs working idle finished
            
    if List.isEmpty remaining && List.isEmpty newWorking 
        then newTotalTime
    else if List.isEmpty remaining || newIdle = 0
        then part2 newFinished newWorking newIdle baseJobTime rules remaining newTotalTime
    else
        let newJobs = [0..newIdle] |> List.fold (fun jobs i -> 
            let available = remaining |> List.except (jobs |> List.map fst)
            match nextAvailable rules newFinished available with
            | None -> jobs
            | Some n -> 
                let time = baseJobTime + 1 + (int n - int 'A')
                (n, time)::jobs) []

        let newRemaining = remaining |> List.except (newJobs |> List.map fst)
        let newIdle = newIdle - List.length newJobs
        let newWorking = List.append newJobs newWorking
        part2 newFinished newWorking newIdle baseJobTime rules newRemaining newTotalTime

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
    let letters = rules |> List.collect (fun (a, b) -> [a;b]) |> List.distinct |> List.sortBy id

    printfn "part 1: %s" <| part1 [] rules letters
    printfn "part 2: %i" <| part2 [] [] 2 0 rules letters -1

    0