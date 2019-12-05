module Day05

open System.IO

let input = (File.ReadAllText ("./inputs/day05.txt")).Split ',' |> Array.map int

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

let mutable out = 0

let part1Ops input = [
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
        mem.[mem.[pIndex + 1]] <- input
        pIndex + 2)
    4, (fun pIndex (mem: int[]) (modes: int[]) ->
       out <- parse mem mem.[pIndex + 1] modes.[0]
       pIndex + 2)
    ]

let part2Ops = [
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
    out <- 0
    let ops = Map.ofList (part1Ops 1)
    intcodeRun ops 0 input |> ignore
    out

let part2 () =
    out <- 0
    let ops = List.append (part1Ops 5) part2Ops |> Map.ofList
    intcodeRun ops 0 input |> ignore
    out