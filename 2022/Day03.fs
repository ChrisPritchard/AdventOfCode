module Day03

open Common
open System

let part1 () =
    readEmbeddedRaw "day03"
    |> Seq.sumBy (fun (line: string) -> 
        let left = Seq.take (line.Length / 2) line |> Set.ofSeq
        let right = Seq.skip (line.Length / 2) line |> Set.ofSeq
        let shared = Set.intersect left right |> Set.toArray |> fun a -> a[0]
        if int shared <= int 'Z' then
            int shared - int 'A' + 27
        else
            int shared - int 'a' + 1)

let part2 () =
    readEmbeddedRaw "day03"
    |> Seq.map Set.ofSeq
    |> Seq.chunkBySize 3
    |> Seq.map (Set.intersectMany >> Set.toArray >> fun a -> a[0])
    |> Seq.sumBy (fun shared -> 
        if int shared <= int 'Z' then
            int shared - int 'A' + 27
        else
            int shared - int 'a' + 1)

