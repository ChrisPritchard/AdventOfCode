module Day03

open System.IO
open Common

let input = 
    File.ReadAllLines ("./inputs/day03.txt")
    |> Array.map (fun s ->
        s |> Seq.map (fun c -> c <> '.') |> Seq.toArray)
        
let path px py =
    let mutable x = 0
    let mutable y = 0
    let mutable count = 0
    let length = Array.length (input.[0])
    while y < (Array.length input) do
        let line = input.[y]
        if line.[x % length] then count <- count + 1
        x <- x + px
        y <- y + py
    count

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