module Day18

open System
open System.IO

let input = File.ReadAllLines "./inputs/day18.txt"

let processed () = 
    input |> Array.map (fun line -> line.Replace(" ", "") |> List.ofSeq)

let asInt (n: char) = uint64 n - 48UL

let part1 () =
    let rec processor acc op rem =
        match rem with
        | [] -> acc, []
        | ')'::rem -> acc, rem
        | '+'::rem -> processor acc '+' rem
        | '*'::rem -> processor acc '*' rem
        | '('::rem ->
            let res, rem = processor 0UL ' ' rem
            match op with
            | '+' -> processor (acc + res) ' ' rem
            | '*' -> processor (acc * res) ' ' rem
            | _ -> processor res ' ' rem
        | n::rem when Char.IsDigit n -> 
            match op with
            | '+' -> processor (acc + asInt n) ' ' rem
            | '*' -> processor (acc * asInt n) ' ' rem
            | _ -> processor (asInt n) ' ' rem
        | _ -> failwithf "unexpected state: %A" rem
    processed ()
    |> Array.sumBy (processor 0UL ' ' >> fst)
   
let part2 () = 
    0