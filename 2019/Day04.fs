module Day04

open Common
open System.IO

let text = File.ReadAllText ("./inputs/day04.txt")

let min = text.Split('-').[0] |> int
let max = text.Split('-').[1] |> int

let part1 () =
    [min..max] 
    |> Seq.filter (fun n ->
        let s = string n
        asString (Seq.sort s) = s && Seq.pairwise s |> Seq.filter (fun (a,b) -> a = b) |> Seq.isEmpty |> not)
    |> Seq.length

let part2 () =
    [min..max] 
    |> Seq.filter (fun n ->
        let s = string n
        if asString (Seq.sort s) <> s then false
        else
            let grouped = Seq.countBy id s
            grouped 
            |> Seq.filter (fun (c, cnt) -> 
                let sc = string c
                cnt > 1 && s.Contains (sc + sc) && not (s.Contains (sc + sc + sc)))
            |> Seq.isEmpty |> not)
    |> Seq.length

// simpler way of doing this, thinking the following morning (note the use of Seq.exists):

(*
let part1 () =
    [min..max] 
    |> Seq.filter (fun n ->
        let s = string n
        asString (Seq.sort s) = s && 
        s |> Seq.exists (fun c -> let sc = string c in s.Contains(sc + sc)))
    |> Seq.length

let part2 () =
    [min..max] 
    |> Seq.filter (fun n ->
        let s = string n
        asString (Seq.sort s) = s && 
        s |> Seq.exists (fun c -> let sc = string c in s.Contains(sc + sc) && not (s.Contains(sc + sc + sc))))
    |> Seq.length
*)