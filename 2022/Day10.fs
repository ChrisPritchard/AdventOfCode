module Day10

open Common
open System

let part1() =
    ((0, (1, 1)), readEmbeddedRaw "day10")
    ||> Seq.fold (fun (sum, (X, cycle)) instruction ->
        if instruction = "noop" then
            let cycle = cycle + 1
            let sum = if (cycle - 20) % 40 = 0 then sum + X * cycle else sum
            sum, (X, cycle)
        else
            let toAdd = Int32.Parse (instruction.Substring(5))
            let cycle = cycle + 2
            let sum = 
                if (cycle - 20) % 40 = 0 then sum + (X + toAdd) * cycle
                else if (cycle - 20) % 40 = 1 then sum + X * (cycle - 1)
                else sum  
            sum, (X + toAdd, cycle))
    |> fst

let part2() =
    0