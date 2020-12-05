module Day05

open System.IO

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
        |> Array.find (fun (_, seats) -> Array.length seats = 7)
    let seatMap = Map.ofArray seats
    [0..7] |> List.pick (fun i -> 
        if not (Map.containsKey (row, i) seatMap) then Some (row * 8 + i) else None)
