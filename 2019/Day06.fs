module Day06

open Common
open System.IO
open System.Collections.Generic

let text = File.ReadAllText ("./inputs/day06.txt")
let cells = File.ReadAllText ("./inputs/day06.txt") |> split "-,"
let numbers = File.ReadAllText ("./inputs/day06.txt") |> split "-," |> Array.map int
let lines = File.ReadAllLines ("./inputs/day06.txt")

let part1 () =
    text

let part2 () =
    text