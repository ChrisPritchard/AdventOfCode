module Day06

open Common
open System
open System.IO

let input = File.ReadAllText("./inputs/day06.txt")

let processed () = splitOn (newline + newline) input

let part1 () = 
    processed () |> Array.sumBy (Seq.filter Char.IsLetter >> Seq.distinct >> Seq.length)

let part2 () =
    processed () |> Array.sumBy (fun g -> 
        let allc = g |> Seq.filter Char.IsLetter |> Seq.distinct
        let rows = g |> split "\r\n" |> Array.map Set.ofSeq
        allc |> Seq.filter (fun c -> rows |> Array.forall (Set.contains c)) |> Seq.length)