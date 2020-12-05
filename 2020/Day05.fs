module Day05

open System
open System.IO
open Common

let input = 
    File.ReadAllLines ("./inputs/day05.txt")

let boardingPass (line: string) =
    let mutable min, max, range, row, seat = 0, 127, 128, 0, 0
    for i = 0 to 6 do
        range <- range / 2
        if line.[i] = 'F' then
            max <- max - range
            row <- max
        else
            min <- min + range
            row <- min

    min <- 0
    max <- 7
    range <- 8

    for i = 7 to 9 do
        range <- range / 2
        if line.[i] = 'L' then
            max <- max - range
            seat <- max
        else
            min <- min + range
            seat <- min
    
    row, seat, row * 8 + seat

let part1 () =
    input
    |> Array.map boardingPass
    |> Array.maxBy (fun (_, _, id) -> id)

let part2 () =
    let allRows =
        input
        |> Array.map boardingPass
        |> Array.groupBy (fun (r, _, _) -> r)
        |> Array.sortBy fst
    let length = Array.length allRows
    let missing = allRows |> Array.skip 1 |> Array.take (length - 2) |> Array.filter (fun (_, seats) -> Array.length seats <> 8)
    missing
