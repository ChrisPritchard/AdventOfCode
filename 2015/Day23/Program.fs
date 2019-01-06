open System.IO
open System

[<EntryPoint>]
let main _ =

    let input = File.ReadAllLines "input.txt"

    let rec runCommand pc (regs: Map<string, int64>) =
        if pc >= input.Length then regs
        else
            let command = input.[pc].Split ([|' ';','|], StringSplitOptions.RemoveEmptyEntries)
            match command.[0] with
            | "hlf" -> 
                let reg = command.[1]
                let regs = Map.add reg (regs.[reg] / 2L) regs
                runCommand (pc + 1) regs
            | "tpl" ->
                let reg = command.[1]
                let regs = Map.add reg (regs.[reg] * 3L) regs
                runCommand (pc + 1) regs
            | "inc" ->
                let reg = command.[1]
                let regs = Map.add reg (regs.[reg] + 1L) regs
                runCommand (pc + 1) regs
            | "jmp" ->
                let offset = int command.[1]
                runCommand (pc + offset) regs
            | "jie" ->
                let reg = command.[1]
                if regs.[reg] % 2L = 0L then
                    let offset = int command.[2]
                    runCommand (pc + offset) regs
                else
                    runCommand (pc + 1) regs
            | "jio" ->
                let reg = command.[1]
                if regs.[reg] = 1L then
                    let offset = int command.[2]
                    runCommand (pc + offset) regs
                else
                    runCommand (pc + 1) regs
            | _ -> failwith "invalid command"

    let regs = ["a", 0L;"b",0L] |> Map.ofList
    let part1 = runCommand 0 regs
    printfn "part 1: %i" <| part1.["b"]

    let regs = ["a", 1L;"b",0L] |> Map.ofList
    let part2 = runCommand 0 regs
    printfn "part 2: %i" <| part2.["b"]

    0