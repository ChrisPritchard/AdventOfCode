module Day04

open Common
open System

let score s =
    let item = Set.toArray s |> fun a -> a[0]
    if int item <= int 'Z' then
        int item - int 'A' + 27
    else
        int item - int 'a' + 1

let part1 () =
    readEmbeddedRaw "day03"
    |> Seq.sumBy (fun (line: string) -> 
        let left = Seq.take (line.Length / 2) line |> Set.ofSeq
        let right = Seq.skip (line.Length / 2) line |> Set.ofSeq
        Set.intersect left right |> score)

let part2 () =
    readEmbeddedRaw "day03"
    |> Seq.map Set.ofSeq
    |> Seq.chunkBySize 3
    |> Seq.map Set.intersectMany
    |> Seq.sumBy score

