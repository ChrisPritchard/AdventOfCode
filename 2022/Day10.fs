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
    // track position of x each cycle - if cycle x is +- 1 of cycle then mark cycle position as positive
    (("\n\t ", (1, 0)), readEmbeddedRaw "day10")
    ||> Seq.fold (fun (lit, (X, cycle)) instruction ->
        if instruction = "noop" then
            let cycle = cycle + 1
            let lit = if cycle % 40 = 0 then lit + "\n\t" else lit
            let lit = if abs (X - (cycle % 40)) <= 1 then lit + "#" else lit + " "
            lit, (X, cycle)
        else
            let toAdd = Int32.Parse (instruction.Substring(5))
            let cycle = cycle + 2
            let lit = 
                let lit = if (cycle - 1) % 40 = 0 then lit + "\n\t" else lit
                if abs (X - ((cycle - 1) % 40)) <= 1 then lit + "#" else lit + " "
            let lit = 
                let lit = if cycle % 40 = 0 then lit + "\n\t" else lit
                if abs ((X + toAdd) - (cycle % 40)) <= 1 then lit + "#" else lit + " "
            lit, (X + toAdd, cycle))
    |> fst
