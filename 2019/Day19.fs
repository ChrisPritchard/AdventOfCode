module Day19

open Common
open System.IO
open System.Collections.Generic

let input = File.ReadAllText "./inputs/day19.txt" |> split ","

let part1 () = 

    let io = Intcode.IO.create ()

    [0L..49L] |> List.sumBy (fun y ->
        [0L..49L] |> List.sumBy (fun x ->
            io.write x
            io.write y
            let mem = Intcode.memFrom input
            Intcode.run 0L 0L mem io |> ignore
            io.read () |> snd)) |> string

let part2 () =

    0