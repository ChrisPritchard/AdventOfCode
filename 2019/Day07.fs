module Day07

open Common
open System.IO
open System.Collections.Generic

let input = (File.ReadAllText ("./inputs/day07.txt")).Split ',' |> Array.map int
//let input = "3,26,1001,26,-4,26,3,27,1002,27,2,27,1,27,26,27,4,27,1001,28,-1,28,1005,28,6,99,0,0,5" |> split "," |> Array.map int

let intcodeRun opcodes startIp startMem =
    let memory = Array.copy startMem
    let parseOp code = 
        let op = code % 100
        (op, code % 1000 / 100, code % 10000 / 1000, code % 100000 / 10000)
    let rec processor ip =
        let opcode, mode1, mode2, mode3 = parseOp memory.[ip]
        if opcode = 99 || not (Map.containsKey opcode opcodes) then ()
        else
            let op = opcodes.[opcode]
            let nextIp = op ip memory [|mode1; mode2; mode3|]
            processor nextIp
    processor startIp
    memory

let parse (mem: int[]) value mode =
    if mode = 0 then mem.[value] else value

let ops (signals: Queue<int>) = Map.ofList [
    1, (fun pIndex (mem: int[]) (modes: int[]) ->
        mem.[mem.[pIndex + 3]] <- 
            parse mem mem.[pIndex + 1] modes.[0] + 
            parse mem mem.[pIndex + 2] modes.[1]
        pIndex + 4)
    2, (fun pIndex (mem: int[]) (modes: int[]) ->
        mem.[mem.[pIndex + 3]] <- 
            parse mem mem.[pIndex + 1] modes.[0] *
            parse mem mem.[pIndex + 2] modes.[1]
        pIndex + 4)
    3, (fun pIndex (mem: int[]) _ ->
        mem.[mem.[pIndex + 1]] <- signals.Dequeue ()
        pIndex + 2)
    4, (fun pIndex (mem: int[]) (modes: int[]) ->
       signals.Enqueue (parse mem mem.[pIndex + 1] modes.[0])
       pIndex + 2)
    5, (fun pIndex (mem: int[]) (modes: int[]) ->
        let value = parse mem mem.[pIndex + 1] modes.[0]
        if value > 0 then parse mem mem.[pIndex + 2] modes.[1] else pIndex + 3)
    6, (fun pIndex (mem: int[]) (modes: int[]) ->
        let value = parse mem mem.[pIndex + 1] modes.[0]
        if value = 0 then parse mem mem.[pIndex + 2] modes.[1] else pIndex + 3)
    7, (fun pIndex (mem: int[]) (modes: int[]) ->
        let value1 = parse mem mem.[pIndex + 1] modes.[0]
        let value2 = parse mem mem.[pIndex + 2] modes.[1]
        mem.[mem.[pIndex + 3]] <- if value1 < value2 then 1 else 0
        pIndex + 4)
    8, (fun pIndex (mem: int[]) (modes: int[]) ->
        let value1 = parse mem mem.[pIndex + 1] modes.[0]
        let value2 = parse mem mem.[pIndex + 2] modes.[1]
        mem.[mem.[pIndex + 3]] <- if value1 = value2 then 1 else 0
        pIndex + 4)
    ]

let part1 () =

    let runWith phase previous = 
        let signals = Queue<int>()
        signals.Enqueue phase
        signals.Enqueue previous

        intcodeRun (ops signals) 0 input |> ignore
        signals.Dequeue ()

    let range = [0..4]

    seq {
        for a in range do
            let aRes = runWith a 0
            for b in List.except [a] range do
                let bRes = runWith b aRes
                for c in List.except [a; b] range do
                    let cRes = runWith c bRes
                    for d in List.except [a; b; c] range do
                        let dRes = runWith d cRes
                        for e in List.except [a; b; c; d] range do
                            yield runWith e dRes
    } |> Seq.max

let part2 () =
    let runWith phase (signals: Queue<int>) = 
        signals.Enqueue phase
        intcodeRun (ops signals) 0 input |> ignore
        signals
    
    let range = [5..9]
    let signals = Queue<int>()
    signals.Enqueue(0)

    seq {
        for a in range do
            let aRes = runWith a signals
            for b in List.except [a] range do
                let bRes = runWith b aRes
                for c in List.except [a; b] range do
                    let cRes = runWith c bRes
                    for d in List.except [a; b; c] range do
                        let dRes = runWith d cRes
                        for e in List.except [a; b; c; d] range do
                            let final = runWith e dRes
                            yield final.Dequeue ()
    } |> Seq.max