module Day04

open System.IO
open System.Collections.Generic

let text = File.ReadAllText ("./inputs/day04.txt")
let cells = text.Split ','
let lines = File.ReadAllLines ("./inputs/day04.txt")

let part1 () =
    lines

let part2 () =
    text