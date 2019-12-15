module Day09

open System.IO

let input = (File.ReadAllText ("./inputs/day09.txt")).Split ','

(*
NOTE: as of this day, the intcode vm is finished from a functional perspective - all subsequent intcode problems use the same vm)
Accordingly, the code has now been moved into its own shared module (Intcode), and is reused here, on 11, 13, 15 etc
*)
    
let part1 () =

    let mem = Intcode.memFrom input
    let io = Intcode.IO.create ()
    io.write 1L
    Intcode.run 0L 0L mem io |> ignore
    io.read () |> snd |> string

let part2 () =
    
    let mem = Intcode.memFrom input
    let io = Intcode.IO.create ()
    io.write 2L
    Intcode.run 0L 0L mem io |> ignore
    io.read () |> snd |> string