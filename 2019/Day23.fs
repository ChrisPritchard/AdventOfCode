module Day23

open Common
open System.IO

let input = File.ReadAllText "./inputs/day23.txt" |> split ","

let computer address =
    let io = Intcode.IO.create ()
    io.write address
    let mem = Intcode.memFrom input
    let rec run ip rb mem =
        let (_, ip, rb, mem) = Intcode.run ip rb mem io
        fun () -> run ip rb mem
    io.output |> Seq.toArray

let part1 () =

    let computers = Array.init 50 (Computer)

let part2 () =

    0