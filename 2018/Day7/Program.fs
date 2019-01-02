open System
open System.IO
open FParsec.CharParsers
open FParsec

let nextAvailable rules finished remaining = 
    remaining 
    |> List.filter (fun char ->
        rules 
        |> List.filter (fun (pre, c) -> c = char && not (Seq.contains pre finished)) 
        |> List.map fst
        |> List.isEmpty)

let rec part1 (finished : char list) rules =
    function
    | [] -> String.Concat (List.rev finished)
    | remaining ->
        let next = nextAvailable rules finished remaining |> Seq.head
        part1 (next::finished) rules (remaining |> List.filter (fun char -> char <> next))

let processJobs working idle finished =
    working 
    |> List.fold (fun (nw, ni, nf) (char, time) ->
        match time with
        | 1 -> nw, ni + 1, char::nf
        | _ -> (char, time - 1)::nw, ni, nf) 
        ([], idle, finished)

let rec part2 (finished : char list) working idle baseJobTime rules remaining totalTime =
    let available = nextAvailable rules finished remaining
    if not (List.isEmpty available) && idle > 0 then
        let newJobs = 
                available 
                |> List.truncate idle 
                |> List.map (fun c -> c, baseJobTime + 1 + (int c - int 'A')) 
        let newRemaining = remaining |> List.except (newJobs |> List.map fst)
        part2 finished (newJobs @ working) (idle - List.length newJobs) baseJobTime rules newRemaining totalTime
    else if List.isEmpty remaining && List.isEmpty working then
        totalTime
    else
        let newWorking, newIdle, newFinished = processJobs working idle finished
        part2 newFinished newWorking newIdle baseJobTime rules remaining (totalTime + 1)

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
    printfn "part 2: %i" <| part2 [] [] 5 60 rules letters 0

    0