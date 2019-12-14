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
    
    let multiply required (amount, needed) =
        let multiplier = required / amount
        needed
        |> Map.toArray
        |> Array.map (fun (s, n) -> s, n * multiplier) 

    let rec expander map =
        let next = 
            map |> Array.collect (fun (s, n) ->
                if s = "ORE" then [|s, n|]
                else multiply n transforms.[s])
        if next |> Array.forall (fun (s, _) -> s = "ORE") then
            Array.sumBy snd next
        else
            expander next

    expander (snd transforms.["FUEL"] |> Map.toArray)

let part2 () =

    0