module Day04

open Common
open System

let part1 () =
    readEmbeddedRaw "day04"
    |> Seq.sumBy (fun (line: string) -> 
        let p = line |> split "-," |> Array.map int
        if p[0] <= p[2] && p[1] >= p[3] then 1
        else if p[2] <= p[0] && p[3] >= p[1] then 1
        else 0)

let part2 () =
    readEmbeddedRaw "day04"
    |> Seq.sumBy (fun (line: string) -> 
        let p = line |> split "-," |> Array.map int
        if p[0] >= p[2] && p[0] <= p[3] then 1
        else if p[1] >= p[2] && p[1] <= p[3] then 1
        else if p[2] >= p[0] && p[2] <= p[1] then 1
        else if p[3] >= p[0] && p[3] <= p[1] then 1
        else 0)

