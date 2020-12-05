module Day05

open System
open System.IO
open Common

let input = 
    File.ReadAllLines ("./inputs/day05.txt")

let boardingPass (line: string) =

    let rowIns = line.[0..6] |> Seq.map ((=) 'F') |> Seq.toList
    let colIns = line.[7..9] |> Seq.map ((=) 'L') |> Seq.toList

    let rec processor ins min max =
        let range = ((max+1)-min)/2
        match ins with
        | [] -> min
        | true::rest ->
            processor rest min (max-range)
        | false::rest ->
            processor rest (min + range) max

    let row = processor rowIns 0 127
    let col = processor colIns 0 7
    (row, col), row * 8 + col

let part1 () =
    input
    |> Array.map boardingPass
    |> Array.maxBy snd
    |> snd

// probably a faster way to do this
// the double sort (groupby and sort) is probably whats causing the cost
// its only 10-20 ms but still

let part2 () =
    let allRows =
        input
        |> Array.map boardingPass
        |> Array.groupBy (fst >> fst)
        |> Array.sortBy fst
    let length = Array.length allRows
    let (row, seats) = 
        allRows 
        |> Array.skip 1 
        |> Array.take (length - 2) 
        |> Array.find (fun (_, seats) -> Array.length seats <> 8)
    let seatMap = Map.ofArray seats
    [0..7] |> List.pick (fun i -> 
        if not (Map.containsKey (row, i) seatMap) then Some (row * 8 + i) else None)
