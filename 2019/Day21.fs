module Day21

open Common
open System.IO

let input = File.ReadAllText "./inputs/day21.txt" |> split ","

let part1 () =

    let io = Intcode.IO.create ()
    let mem = Intcode.memFrom input
    Intcode.run 0L 0L mem io |> ignore
    io.read () |> snd |> (=) 1L

let part2 () =

    0