module Day10

open Common
open System

let processed = readEmbedded "day10"

let init () =
    processed |> Array.length |> ignore

type Res = 
    | Closed
    | Continue of char list
    | Result of char

let beg c = Array.contains c [|'(';'[';'<';'{'|]
let opp = Map.ofArray [|')','(';']','[';'}','{';'>','<'|]

let part1 () =
    let scores = Map.ofArray [|')',3;']',57;'}',1197;'>',25137|]
    let rec score queue rem =
        match rem, queue with
        | [], _ -> 0
        | c::rem, _ when beg c -> score (c::queue) rem
        | c::rem, o::queue when o = opp[c] -> score queue rem
        | c::rem, _ -> scores[c]                

    processed
    |> Array.sumBy (fun s -> score [] (List.ofSeq s))

let part2 () =
    let scores = Map.ofArray [|'(',1L;'[',2L;'{',3L;'<',4L|]
    let rec score queue = queue |> List.fold (fun v c -> v * 5L + scores[c]) 0L |> Some
    let rec find queue rem =
        match rem, queue with
        | [], [] -> None
        | [], _ -> score queue
        | c::rem, _ when beg c ->
            find (c::queue) rem
        | c::rem, o::queue when o = opp[c] -> find queue rem
        | _ -> None

    processed
    |> Array.choose (fun s -> find [] (List.ofSeq s))
    |> Array.sort
    |> fun a -> a[a.Length / 2]
