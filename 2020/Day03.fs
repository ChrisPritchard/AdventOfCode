module Day03

open System.IO

let input = 
    File.ReadAllLines ("./inputs/day03.txt")
    |> Array.map (fun s ->
        s |> Seq.map ((<>) '.') |> Seq.toArray)
        
let path px py =
    let length = Array.length input.[0]
    let lines = Array.length input
    let rec counter x y c =
        if y >= lines then c
        else
            let count = if input.[y].[x % length] then c + 1 else c
            counter (x + px) (y + py) count
    counter 0 0 0
        
let part1 () =
    path 3 1

let part2 () =
    [
        path 1 1
        path 3 1
        path 5 1
        path 7 1
        path 1 2
    ] |> List.reduce (*)
