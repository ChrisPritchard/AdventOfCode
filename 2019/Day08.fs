module Day08

open Common
open System.IO
open System.Collections.Generic

let input = File.ReadAllText ("./inputs/day08.txt")

let width, height = 25, 6

let part1 () =
    input 
    |> Seq.chunkBySize (width * height)
    |> Seq.map (fun layer ->
        layer |> Seq.countBy id |> Map.ofSeq)
    |> Seq.minBy (fun counts -> counts.['0'])
    |> fun counts -> counts.['1'] * counts.['2']

let part2 () =
    let layers =
        input 
        |> Seq.chunkBySize (width * height)
        |> Seq.map (fun layer ->
            layer |> Seq.chunkBySize width |> Seq.map Seq.toArray |> Seq.toArray)
        |> Seq.toArray

    asString <| seq {
        yield '\n'
        for y = 0 to height - 1 do
            for x = 0 to width - 1 do
                let pixel = 
                    layers 
                    |> Seq.tryPick (fun (layer: char [] []) -> if layer.[y].[x] <> '2' then Some layer.[y].[x] else None)
                    |> Option.defaultValue '2'            
                yield 
                    if pixel = '0' then ' ' elif pixel = '1' then '#' else '.'
            yield '\n'
    }
    