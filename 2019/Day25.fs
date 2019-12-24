module Day25

open Common
open System.IO
open System.Collections.Generic

let input = File.ReadAllText "./inputs/day25.txt" |> split ","

let run instructions =
    let io = Intcode.IO.create ()
    instructions |> Array.iter (fun s ->
        s |> Seq.iter (int64 >> io.write)
        io.write 10L)
    let mem = Intcode.memFrom input
    Intcode.run 0L 0L mem io |> ignore
    io.output |> Seq.toArray

let part1 () =

    0

let part2 () =

    0