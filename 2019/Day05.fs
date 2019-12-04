module Day05

open Common
open System.IO
open System.Collections.Generic

let text = File.ReadAllText ("./inputs/day05.txt")
let cells = text.Split ','
let numbers = text |> split "-," |> Array.map int
let lines = File.ReadAllLines ("./inputs/day05.txt")

let part1 () =
    text

let part2 () =
    text