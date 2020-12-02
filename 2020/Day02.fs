module Day02

open System.IO
open Common

let input = File.ReadAllLines ("./inputs/day02.txt")

let part1 () =
    input 
    |> Array.filter (fun s ->
        let parts = split "- :" s
        let dict = parts.[3] |> Seq.countBy id |> Map.ofSeq
        match Map.tryFind (char parts.[2]) dict with
        | None -> false
        | Some count ->
            count >= int parts.[0] && count <= int parts.[1])
    |> Array.length

let part2 () =
    input 
    |> Array.filter (fun s ->
        let parts = split "- :" s
        let dict = parts.[3] |> Array.ofSeq
        let target = char parts.[2]
        let first = dict.[int parts.[0] - 1]
        let second = dict.[int parts.[1] - 1]
        first.Equals(target) <> second.Equals(target))
    |> Array.length