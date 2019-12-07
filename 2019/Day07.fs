module Day07

open Common
open System.IO
open System.Collections.Generic

let startMemory = (File.ReadAllText ("./inputs/day07.txt")).Split ',' |> Array.map int
//let startMemory = "3,52,1001,52,-5,52,3,53,1,52,56,54,1007,54,5,55,1005,55,26,1001,54,-5,54,1105,1,12,1,53,54,53,1008,54,0,55,1001,55,1,55,2,53,55,53,4,53,1001,56,-1,56,1005,56,6,99,0,0,0,0,10" |> split "," |> Array.map int

type State = Running | Halted | Blocked of int * int[]

let intcodeRun opcodes startIp (memory: int[]) input output =
    let parseOp code = 
        let op = code % 100
        (op, code % 1000 / 100, code % 10000 / 1000, code % 100000 / 10000)
    let rec processor ip =
        let opcode, mode1, mode2, mode3 = parseOp memory.[ip]
        if opcode = 99 || not (Map.containsKey opcode opcodes) then Halted
        else
            let op = opcodes.[opcode]
            let nextIp, nextState = op ip memory [|mode1; mode2; mode3|] input output
            if nextState = Running then processor nextIp else nextState
    processor startIp

let parse (mem: int[]) value mode =
    if mode = 0 then mem.[value] else value

let ops = Map.ofList [
    1, (fun pIndex (mem: int[]) (modes: int[]) _ _ ->
        mem.[mem.[pIndex + 3]] <- 
            parse mem mem.[pIndex + 1] modes.[0] + 
            parse mem mem.[pIndex + 2] modes.[1]
        pIndex + 4, Running)
    2, (fun pIndex (mem: int[]) (modes: int[]) _ _ ->
        mem.[mem.[pIndex + 3]] <- 
            parse mem mem.[pIndex + 1] modes.[0] *
            parse mem mem.[pIndex + 2] modes.[1]
        pIndex + 4, Running)
    3, (fun pIndex (mem: int[]) _ (input: Queue<int>) _->
        if input.Count = 0 then
            pIndex, Blocked (pIndex, mem)
        else
            mem.[mem.[pIndex + 1]] <- input.Dequeue ()
            pIndex + 2, Running)
    4, (fun pIndex (mem: int[]) (modes: int[]) _ (output: Queue<int>) ->
       output.Enqueue (parse mem mem.[pIndex + 1] modes.[0])
       pIndex + 2, Running)
    5, (fun pIndex (mem: int[]) (modes: int[]) _ _ ->
        let value = parse mem mem.[pIndex + 1] modes.[0]
        let nextIp = 
            if value > 0 then parse mem mem.[pIndex + 2] modes.[1] 
            else pIndex + 3
        nextIp, Running)
    6, (fun pIndex (mem: int[]) (modes: int[]) _ _ ->
        let value = parse mem mem.[pIndex + 1] modes.[0]
        let nextIp =
            if value = 0 then parse mem mem.[pIndex + 2] modes.[1] 
            else pIndex + 3
        nextIp, Running)
    7, (fun pIndex (mem: int[]) (modes: int[]) _ _ ->
        let value1 = parse mem mem.[pIndex + 1] modes.[0]
        let value2 = parse mem mem.[pIndex + 2] modes.[1]
        mem.[mem.[pIndex + 3]] <- if value1 < value2 then 1 else 0
        pIndex + 4, Running)
    8, (fun pIndex (mem: int[]) (modes: int[]) _ _ ->
        let value1 = parse mem mem.[pIndex + 1] modes.[0]
        let value2 = parse mem mem.[pIndex + 2] modes.[1]
        mem.[mem.[pIndex + 3]] <- if value1 = value2 then 1 else 0
        pIndex + 4, Running)
    ]

let part1 () =

    let runWith phase previous = 
        let input = Queue<int>([phase;previous])
        let output = Queue<int>()

        let memory = Array.copy startMemory
        intcodeRun ops 0 memory input output |> ignore
        output.Dequeue ()

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
    
    let range = [5..9]

    seq {
        for a in range do
            for b in List.except [a] range do
                for c in List.except [a; b] range do
                    for d in List.except [a; b; c] range do
                        for e in List.except [a; b; c; d] range do

                            let aInput = Queue<int>([a;0])
                            let bInput = Queue<int>([b])
                            let cInput = Queue<int>([c])
                            let dInput = Queue<int>([d])
                            let eInput = Queue<int>([e])

                            let memory = [|Array.copy startMemory; Array.copy startMemory; Array.copy startMemory; Array.copy startMemory; Array.copy startMemory|]
                            let mutable aState, bState, cState, dState, eState = Running, Running, Running, Running, Running
                            
                            let mutable finished = false
                            while not finished do
                                match aState with
                                | Running ->
                                    aState <- intcodeRun ops 0 memory.[0] aInput bInput
                                | Blocked (ip, mem) ->
                                    aState <- intcodeRun ops ip mem aInput bInput
                                | _ -> ()

                                match bState with
                                | Running ->
                                    bState <- intcodeRun ops 0 memory.[1] bInput cInput
                                | Blocked (ip, mem) ->
                                    bState <- intcodeRun ops ip mem bInput cInput
                                | _ -> ()

                                match cState with
                                | Running ->
                                    cState <- intcodeRun ops 0 memory.[2] cInput dInput
                                | Blocked (ip, mem) ->
                                    cState <- intcodeRun ops ip mem cInput dInput
                                | _ -> ()

                                match dState with
                                | Running ->
                                    dState <- intcodeRun ops 0 memory.[3] dInput eInput
                                | Blocked (ip, mem) ->
                                    dState <- intcodeRun ops ip mem dInput eInput
                                | _ -> ()

                                match eState with
                                | Running ->
                                    eState <- intcodeRun ops 0 memory.[4] eInput aInput
                                | Blocked (ip, mem) ->
                                    eState <- intcodeRun ops ip mem eInput aInput
                                | _ -> ()

                                if eState = Halted && aInput.Count > 0 then
                                    yield aInput.Dequeue ()
                                    finished <- true
                            
    } |> Seq.max