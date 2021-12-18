module Day18

open Common
open System

let processed = readEmbedded "day18"

let init () =
    processed |> Array.length |> ignore

type SnailFishNumber =
    | Pair of int * SnailFishNumber * SnailFishNumber
    | Regular of int

let parse (s:string) =
    let l = Seq.toList s |> List.tail
    let rec parse level left right acc rem =
        match rem, left, right with
        | ']'::rem, Some a, None when acc <> "" -> 
            let right = Regular (int acc)
            Pair (level, a, right), rem
        | ']'::rem, Some a, Some b -> 
            Pair (level, a, b), rem
        | '['::rem, None, _ -> 
            let left, rem = parse (level + 1) None None "" rem
            parse level (Some left) None "" rem
        | '['::rem, Some a, None -> 
            let right, rem = parse (level + 1) None None "" rem
            parse level (Some a) (Some right) "" rem
        | ','::rem, None, None when acc <> "" -> 
            let left = Regular (int acc)
            parse level (Some left) None "" rem
        | ','::rem, Some left, None -> 
            parse level (Some left) None "" rem
        | c::rem, _, _ when Char.IsDigit c -> parse level left right (acc + string c) rem
        | c, _, _ -> failwith (sprintf "shouldn't be reached: %A %A %A %A" c left right acc)
    parse 1 None None "" l |> fst
    
let add a b =
    let rec increment n =
        match n with
        | Regular _ -> n
        | Pair (l, a, b) -> Pair (l + 1, increment a, increment b)
    Pair (1, increment a, increment b)

let reduce number =
    let rec reduce level n =
        match n with
        | Regular v when v > 9 -> Pair (level + 1, Regular (floor (float v / 2) |> int), Regular (ceil (float v / 2) |> int)), Some n
        | Regular _ -> n, None
        | Pair (l, a, b) when l > 4 -> Regular 0, Some n
        | Pair (l, a, b) -> 
            match reduce (l + 1) a with
            | v, Some (Pair (_, a, b))

let part1 () =
    processed |> Array.map parse |> Array.reduce add

let part2 () =
    0