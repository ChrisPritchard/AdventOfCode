module Day07

open Common
open System
open System.IO

let input = 
    File.ReadAllText("./inputs/day07.txt")
    |> splitOn (newline + newline)

let part1 () = 
    input |> Array.sumBy (Seq.filter Char.IsLetter >> Seq.distinct >> Seq.length)

let part2 () =
    input |> Array.sumBy (fun g -> 
        let allc = g |> Seq.filter Char.IsLetter |> Seq.distinct
        let rows = g |> split "\r\n" |> Array.map Set.ofSeq
        allc |> Seq.filter (fun c -> rows |> Array.forall (Set.contains c)) |> Seq.length)