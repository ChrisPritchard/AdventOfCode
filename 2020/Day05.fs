module Day05

open Common
open System.IO
open System

let input = File.ReadAllLines ("./inputs/day05.txt")

let processed () = 
    let boardingPass (line) =
        line 
        |> Seq.map (function 'F' | 'L' -> '0' | _ -> '1') |> asString
        |> fun s -> Convert.ToInt32(s, 2)
    Array.map boardingPass input

let part1 () = processed () |> Array.max

let part2 () =
    let all = processed () |> Set.ofArray
    let inset i = Set.contains i all
    [0..1024] |> List.find (fun i -> not (inset i) && inset (i - 1) && inset (i + 1))

(*
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

let input = 
    File.ReadAllLines ("./inputs/day05.txt")
    |> Array.map boardingPass

let part1 () =
    input
    |> Array.maxBy snd
    |> snd

let part2 () =
    let (row, seats) =
        input
        |> Array.groupBy (fst >> fst)
        |> Array.find (fun (_, seats) -> Array.length seats = 7) // makes an assumption that first/last rows are missing more than one seat
    let seatMap = Map.ofArray seats
    [0..7] |> List.pick (fun i -> 
        if not (Map.containsKey (row, i) seatMap) then Some (row * 8 + i) else None)
*)