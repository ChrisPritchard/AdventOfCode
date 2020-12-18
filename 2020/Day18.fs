module Day18

open System
open System.IO

let input = File.ReadAllLines "./inputs/day18.txt"

let processed () = 
    input |> Array.map (fun line -> line.Replace(" ", "") |> List.ofSeq)

let asInt (n: char) = uint64 n - 48UL

let part1 () =
    let rec processor acc op rem =
        match rem, op with
        | [], _ -> acc, []
        | ')'::rem, _ -> acc, rem
        | '+'::rem, _ -> processor acc '+' rem
        | '*'::rem, _ -> processor acc '*' rem
        | '('::rem, '+' -> 
            let res, rem = processor 0UL ' ' rem
            processor (acc + res) ' '  rem
        | '('::rem, '*' -> 
            let res, rem = processor 0UL ' ' rem
            processor (acc * res) ' ' rem
        | '('::rem, _ -> 
            let res, rem = processor 0UL ' ' rem
            processor res ' ' rem
        | n::rem, '+' when Char.IsDigit n -> processor (acc + asInt n) ' ' rem
        | n::rem, '*' when Char.IsDigit n -> processor (acc * asInt n) ' ' rem
        | n::rem, _ when Char.IsDigit n -> processor (asInt n) ' ' rem
        | n::rem, _ -> failwith (sprintf "unexpected input: %A" n)
    processed ()
    |> Array.sumBy (fun line -> processor 0UL ' ' line |> fst)
   
let part2 () = 
    0