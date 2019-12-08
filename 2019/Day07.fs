module Day07

open Common
open System
open System.IO
open System.Collections.Generic

let startMemory = (File.ReadAllText ("./inputs/day07.txt")).Split ',' |> Array.map int
//let startMemory = "3,52,1001,52,-5,52,3,53,1,52,56,54,1007,54,5,55,1005,55,26,1001,54,-5,54,1105,1,12,1,53,54,53,1008,54,0,55,1001,55,1,55,2,53,55,53,4,53,1001,56,-1,56,1005,56,6,99,0,0,0,0,10" |> split "," |> Array.map int

type State = Running | Halted | Blocked of int * int[]

let intcodeRun opcodes startIp (memory: int[]) input output =
    let parseOp code = 
        let op = code % 100
        (op, code % 1000 / 100, code % 10000 / 1000)
    let rec processor ip =
        let opcode, mode1, mode2 = parseOp memory.[ip]
        let v1 () = let v = memory.[ip + 1] in if mode1 = 0 then memory.[v] else v
        let v2 () = let v = memory.[ip + 2] in if mode2 = 0 then memory.[v] else v
        try
            let op = Map.find opcode opcodes
            let nextIp, nextState = op ip memory v1 v2 input output
            if nextState = Running then processor nextIp else nextState
        with
        | :? KeyNotFoundException | :? IndexOutOfRangeException  -> Halted
    processor startIp

let parse (mem: int[]) value mode =
    if mode = 0 then mem.[value] else value

let ops = Map.ofList [
    99, (fun ip _ _ _ _ _ -> // halt
        ip, Halted)

    1, (fun ip (mem: int[]) v1 v2 _ _ -> // Add
        mem.[mem.[ip + 3]] <- v1() + v2()
        ip + 4, Running)

    2, (fun ip (mem: int[]) v1 v2 _ _ -> // Mul
        mem.[mem.[ip + 3]] <- v1() * v2()
        ip + 4, Running)

    3, (fun ip (mem: int[]) _ _ (input: Queue<int>) _-> // Read or block
        if input.Count = 0 then
            ip, Blocked (ip, mem)
        else
            mem.[mem.[ip + 1]] <- input.Dequeue ()
            ip + 2, Running)

    4, (fun ip _ v1 _ _ (output: Queue<int>) -> // Write
       output.Enqueue (v1())
       ip + 2, Running)

    5, (fun ip _ v1 v2 _ _ -> // jump if greater
        let nextIp = if v1() > 0 then v2() else ip + 3
        nextIp, Running)

    6, (fun ip _ v1 v2 _ _ -> // jump if equal
        let nextIp = if v1() = 0 then v2() else ip + 3
        nextIp, Running)

    7, (fun ip (mem: int[]) v1 v2 _ _ -> // set if less
        mem.[mem.[ip + 3]] <- if v1() < v2() then 1 else 0
        ip + 4, Running)

    8, (fun ip (mem: int[]) v1 v2 _ _ -> // set if equal
        mem.[mem.[ip + 3]] <- if v1() = v2() then 1 else 0
        ip + 4, Running)
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

                            let rec amplifier aState bState cState dState eState =
                                let aState =
                                    match aState with
                                    | Running ->
                                        intcodeRun ops 0 (Array.copy startMemory) aInput bInput
                                    | Blocked (ip, mem) ->
                                        intcodeRun ops ip mem aInput bInput
                                    | _ -> aState

                                let bState =
                                    match bState with
                                    | Running ->
                                        intcodeRun ops 0 (Array.copy startMemory) bInput cInput
                                    | Blocked (ip, mem) ->
                                        intcodeRun ops ip mem bInput cInput
                                    | _ -> bState

                                let cState =
                                    match cState with
                                    | Running ->
                                        intcodeRun ops 0 (Array.copy startMemory) cInput dInput
                                    | Blocked (ip, mem) ->
                                        intcodeRun ops ip mem cInput dInput
                                    | _ -> cState

                                let dState =
                                    match dState with
                                    | Running ->
                                        intcodeRun ops 0 (Array.copy startMemory) dInput eInput
                                    | Blocked (ip, mem) ->
                                        intcodeRun ops ip mem dInput eInput
                                    | _ -> dState

                                let eState =
                                    match eState with
                                    | Running ->
                                        intcodeRun ops 0 (Array.copy startMemory) eInput aInput
                                    | Blocked (ip, mem) ->
                                        intcodeRun ops ip mem eInput aInput
                                    | _ -> eState

                                if eState = Halted && aInput.Count > 0 then
                                    aInput.Dequeue ()
                                else
                                    amplifier aState bState cState dState eState

                            yield amplifier Running Running Running Running Running
    } |> Seq.max