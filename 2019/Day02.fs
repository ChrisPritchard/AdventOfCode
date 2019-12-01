module Day02

open System.IO
open Common

let input = File.ReadAllLines ("./inputs/day02.txt")

let part1 () =
    input.Length + 1

let part2 () =
    input.Length + 2