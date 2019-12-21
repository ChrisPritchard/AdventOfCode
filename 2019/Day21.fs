module Day21

open Common
open System.IO

let input = File.ReadAllText "./inputs/day21.txt" |> split ","

let part1 () =

    let run instructions =
        let io = Intcode.IO.create ()
        instructions |> Array.iter (fun s ->
            s |> Seq.iter (int64 >> io.write)
            io.write 10L)
        let mem = Intcode.memFrom input
        Intcode.run 0L 0L mem io |> ignore
        io.output |> Seq.toArray

    let result = run [|"WALK"|]
    result |> Array.iter (int >> char >> printf "%c")
    0

let part2 () =

    0