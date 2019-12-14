module Day14

open Common
open System.IO
open System.Collections.Generic

let input = File.ReadAllLines "./inputs/day14.txt"

let transforms =
    input
    |> Array.map (fun s -> 
        s
        |> split " ,=>"
        |> fun a ->
            a.[a.Length - 1], 
                (int a.[a.Length - 2], 
                    a 
                    |> Array.truncate (a.Length - 2) 
                    |> Array.chunkBySize 2 
                    |> Array.map (fun b -> b.[1], int b.[0])
                    |> Map.ofArray))
    |> Map.ofArray

let part1 () =    
    
    transforms

let part2 () =

    0