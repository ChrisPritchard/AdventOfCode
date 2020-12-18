module Day18

open System
open System.IO

let input = File.ReadAllLines "./inputs/day18.txt"

let processed () = 
    input |> Array.map (fun line -> line.Replace(" ", "") |> List.ofSeq)

let part1 () =
    processed () |> fun a -> a.[0] |> printfn "%A"
    0
   
let part2 () = 
    0