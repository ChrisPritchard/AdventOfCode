module Day07

open Common
open System
open System.IO
open System.Collections.Generic

let startMemory = (File.ReadAllText ("./inputs/day07.txt")).Split ',' |> Array.map int
//let startMemory = "3,52,1001,52,-5,52,3,53,1,52,56,54,1007,54,5,55,1005,55,26,1001,54,-5,54,1105,1,12,1,53,54,53,1008,54,0,55,1001,55,1,55,2,53,55,53,4,53,1001,56,-1,56,1005,56,6,99,0,0,0,0,10" |> split "," |> Array.map int

type State = Running | Halted | Blocked

let intcodeRun opcodes startIp (memory: int[]) read write =
    let parseOp code = 
        let op = code % 100
        (op, code % 1000 / 100, code % 10000 / 1000)
    let rec processor ip =
        let opcode, mode1, mode2 = parseOp memory.[ip]
        let v1 () = let v = memory.[ip + 1] in if mode1 = 0 then memory.[v] else v
        let v2 () = let v = memory.[ip + 2] in if mode2 = 0 then memory.[v] else v
        let set i v = let o = memory.[i] in memory.[o] <- v
        try
            let op = Map.find opcode opcodes
            let nextIp, nextState = op ip v1 v2 set read write
            if nextState = Running then processor nextIp else nextState, nextIp
        with
        | :? KeyNotFoundException | :? IndexOutOfRangeException  -> Halted, ip
    let finalState, finalIp = processor startIp
    finalState, finalIp, memory

let parse (mem: int[]) value mode =
    if mode = 0 then mem.[value] else value

let ops = Map.ofList [
    99, (fun ip _ _ _ _ _ -> // halt
        ip, Halted)

    1, (fun ip v1 v2 set _ _ -> // Add
        v1() + v2() |> set (ip + 3)
        ip + 4, Running)

    2, (fun ip v1 v2 set _ _ -> // Mul
        v1() * v2() |> set (ip + 3)
        ip + 4, Running)

    3, (fun ip _ _ set read _-> // Read or block
        let (canRead, value) = read ()
        if canRead then
            set (ip + 1) value
            ip + 2, Running
        else
            ip, Blocked)

    4, (fun ip v1 _ _ _ write -> // Write
       write <| v1()
       ip + 2, Running)

    5, (fun ip v1 v2 _ _ _ -> // jump if greater
        let nextIp = if v1() > 0 then v2() else ip + 3
        nextIp, Running)

    6, (fun ip v1 v2 _ _ _ -> // jump if equal
        let nextIp = if v1() = 0 then v2() else ip + 3
        nextIp, Running)

    7, (fun ip v1 v2 set _ _ -> // set if less
        set (ip + 3) <| if v1() < v2() then 1 else 0
        ip + 4, Running)

    8, (fun ip v1 v2 set _ _ -> // set if equal
        set (ip + 3) <| if v1() = v2() then 1 else 0
        ip + 4, Running)
    ]

let read (queue: Queue<int>) = 
    fun () -> if queue.Count > 0 then true, queue.Dequeue () else false, 0

let part1 () =

    let runWith phase previous = 
        let memory = Array.copy startMemory

        let input = Queue<int>([phase;previous])
        let output = Queue<int>()
        intcodeRun ops 0 memory (read input) (output.Enqueue) |> ignore
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
                                    | Halted, _, _ -> aState
                                    | _, ip, mem ->
                                        intcodeRun ops ip mem (read aInput) (bInput.Enqueue)

                                let bState =
                                    match bState with
                                    | Halted, _, _ -> bState
                                    | _, ip, mem ->
                                        intcodeRun ops ip mem (read bInput) (cInput.Enqueue)

                                let cState =
                                    match cState with
                                    | Halted, _, _ -> cState
                                    | _, ip, mem ->
                                        intcodeRun ops ip mem (read cInput) (dInput.Enqueue)

                                let dState =
                                    match dState with
                                    | Halted, _, _ -> dState
                                    | _, ip, mem ->
                                        intcodeRun ops ip mem (read dInput) (eInput.Enqueue)

                                let eState =
                                    match eState with
                                    | Halted, _, _ -> eState
                                    | _, ip, mem ->
                                        intcodeRun ops ip mem (read eInput) (aInput.Enqueue)

                                match eState with
                                | Halted, _, _ ->
                                    if aInput.Count > 0 then aInput.Dequeue ()
                                    else 0
                                | _ ->
                                    amplifier aState bState cState dState eState

                            let startState () = Running, 0, Array.copy startMemory
                            yield amplifier (startState ()) (startState ()) (startState ()) (startState ()) (startState ())
    } |> Seq.max