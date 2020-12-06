module Day06

open Common
open System.IO

let input = 
    File.ReadAllText("./inputs/day06.txt")
    |> splitOn "\r\n\r\n"

let part1 () = 
    input |> Array.sumBy (fun g -> g |> Seq.filter (fun c -> System.Char.IsLetter(c)) |> Seq.distinct |> Seq.length)

let part2 () =
    input |> Array.sumBy (fun g -> 
        let allc = g |> Seq.filter (fun c -> System.Char.IsLetter(c)) |> Seq.distinct
        let rows = g |> split "\r\n" |> Array.map (Set.ofSeq)
        allc |> Seq.filter (fun c -> rows |> Array.forall (Set.contains c)) |> Seq.length)