module Day21

open Common
open System.IO

let input = File.ReadAllText "./inputs/day21.txt" |> split ","

let run instructions =
    let io = Intcode.IO.create ()
    instructions |> Array.iter (fun s ->
        s |> Seq.iter (int64 >> io.write)
        io.write 10L)
    let mem = Intcode.memFrom input
    Intcode.run 0L 0L mem io |> ignore
    io.output |> Seq.toArray

let part1 () =

    let instructions = [|
        "NOT A J"
        "NOT B T"
        "OR T J"
        "NOT C T"
        "OR T J"
        "AND D J"
        "WALK"
        |]

    let result = run instructions
    Array.last result |> string

let part2 () =

    let instructions = [|
        "NOT A J"
        "NOT B T"
        "OR T J"
        "NOT C T"
        "OR T J"

        "AND D J"

        "AND H J"
        "OR E J"
        "AND I J"

        "RUN"
        |]

    let result = run instructions
    //Array.last result |> string
    result |> Array.iter (char >> printf "%c")
    0