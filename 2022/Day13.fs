module Day13

open Common
open System

type Packet =
    | OpenList
    | Digit of int
    | CloseList

let textToList (s: string) =
    s.Replace("[", "[,").Replace("]", ",]") // just making parsing easier
    |> split "," 
    |> Seq.map (fun c -> 
        match c with 
        | "[" -> OpenList 
        | "]" -> CloseList 
        | _ -> Digit (Int32.Parse c))
    |> Seq.toList

let inOrder listA listB =
    let rec comparer aList bList =
        match aList, bList with
        | a::remA, b::remB when a = b -> comparer remA remB
        | Digit a::_, Digit b::_ when a < b -> true
        | Digit a::_, Digit b::_ when a > b -> false
        | OpenList::_, Digit b::remB -> comparer aList (OpenList::Digit b::CloseList::remB)
        | Digit a::remA, OpenList::_ -> comparer (OpenList::Digit a::CloseList::remA) bList
        | CloseList::_, _ -> true
        | _, CloseList::_ -> false
        | _ -> false
    comparer (textToList listA) (textToList listB)

let part1 () =
    readEmbedded "day13"
    |> Array.chunkBySize 3
    |> Array.indexed
    |> Array.filter (fun (_, lists) -> inOrder lists[0] lists[1])
    |> Array.map (fst >> (+) 1)
    |> Array.sum
    
let part2 () =
    let sorted = 
        readEmbedded "day13"
        |> Array.chunkBySize 3 
        |> Array.collect (fun a -> [|a[0]; a[1]|])
        |> Array.append [| "[[2]]"; "[[6]]" |]
        |> Array.sortWith (fun a b -> if inOrder a b then -1 else 1)
    (Array.findIndex ((=) "[[2]]") sorted + 1) * (Array.findIndex ((=) "[[6]]") sorted + 1)