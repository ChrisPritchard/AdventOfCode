module Day18

open Common
open System

let processed = readEmbedded "day18"

let init () =
    processed |> Array.length |> ignore

type SnailFishNumber =
    | Pair of SnailFishNumber * SnailFishNumber
    | Regular of int

let parse (s:string) =
    let l = Seq.toList s |> List.tail
    let rec parse left right acc rem =
        match rem, left, right with
        | ']'::rem, Some a, None when acc <> "" -> 
            let right = Regular (int acc)
            Pair (a, right), rem
        | ']'::rem, Some a, Some b -> 
            Pair (a, b), rem
        | '['::rem, None, _ -> 
            let left, rem = parse None None "" rem
            parse (Some left) None "" rem
        | '['::rem, Some a, None -> 
            let right, rem = parse None None "" rem
            parse (Some a) (Some right) "" rem
        | ','::rem, None, None when acc <> "" -> 
            let left = Regular (int acc)
            parse (Some left) None "" rem
        | ','::rem, Some left, None -> 
            parse (Some left) None "" rem
        | c::rem, _, _ when Char.IsDigit c -> parse left right (acc + string c) rem
        | c, _, _ -> failwith (sprintf "shouldn't be reached: %A %A %A %A" c left right acc)
    parse None None "" l |> fst
    

let part1 () =
    parse processed[0]

let part2 () =
    0