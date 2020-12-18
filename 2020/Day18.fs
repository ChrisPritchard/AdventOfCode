module Day18

open Common
open System
open System.IO

let input = File.ReadAllLines "./inputs/day18.txt"

let processed () = 
    input |> Array.map (fun s -> s.Replace (")", " )") + " " |> List.ofSeq)

let rec processor acc op curr rem =
    //printfn "%A" acc
    match rem with
    | [] -> acc, []
    | ')'::rem -> acc, rem
    | '+'::rem -> processor acc '+' "" rem
    | '*'::rem -> processor acc '*' "" rem
    | '('::rem ->
        let res, rem = processor 0UL ' ' "" rem
        match op with
        | '+' -> processor (acc + res) ' ' "" rem
        | '*' -> processor (acc * res) ' ' "" rem
        | _ -> processor res ' ' "" rem
    | n::rem when Char.IsDigit n -> processor acc op (curr + string n) rem
    | ' '::rem when curr <> "" ->
        match op with
        | '+' -> processor (acc + uint64 curr) ' ' "" rem
        | '*' -> processor (acc * uint64 curr) ' ' "" rem
        | _ -> processor (uint64 curr) ' ' "" rem
    | ' '::rem -> processor acc op curr rem
    | _ -> failwithf "unexpected state: %A" rem

let part1 () =
    processed ()
    |> Array.sumBy (processor 0UL ' ' "" >> fst)

let precedence line =
    let valueOf s = 
        s 
        |> split "*" 
        |> Array.map (fun seg -> "("+seg+")") 
        |> String.concat "*"
        |> fun s -> s.Replace (")", " )") |> Seq.toList |> processor 0UL ' ' "" |> fst
    let rec folder acc rem =
        match rem with
        | [] -> valueOf acc, []
        | ')'::rem -> valueOf acc, rem
        | '('::rem -> 
            let v, rem = folder "" rem
            folder (acc + string v) rem
        | c::rem -> folder (acc + string c) rem
    folder "" line |> fst
   
let part2 () = 
    processed ()
    |> Array.sumBy precedence