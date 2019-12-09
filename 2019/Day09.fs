module Day09

open Common
open System
open System.IO
open System.Collections.Generic

let startMemory = (File.ReadAllText ("./inputs/day09.txt")).Split ',' |> Array.map int64
//let startMemory = "109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99" |> split "," |> Array.map (int64)

type State = Running | Halted | Blocked

let intcodeRun opcodes (memory: Dictionary<int64, int64>) read write =
    let parseOp code = 
        let op = code % 100L
        (op, code % 1000L / 100L, code % 10000L / 1000L, code % 100000L / 10000L)
    let rec processor ip rb =
        let opcode, mode1, mode2, mode3 = parseOp memory.[ip]
        let v1 () = 
            let v = memory.[ip + 1L] 
            if mode1 = 0L then memory.[v] 
            elif mode1 = 2L then memory.[rb + v]
            else v
        let v2 () = 
            let v = memory.[ip + 2L] 
            if mode2 = 0L then memory.[v] 
            elif mode2 = 2L then memory.[rb + v]
            else v
        let set i v = 
            let o = memory.[ip + i]
            let sidx = 
                if i = 1L && mode1 = 2L then rb + o
                elif i = 2L && mode2 = 2L then rb + o
                elif i = 3L && mode3 = 2L then rb + o
                else o
            memory.[sidx] <- v
        let op = Map.find opcode opcodes
        let nextIp, nextRb, nextState = op ip rb v1 v2 set read write
        if nextState = Running then processor nextIp nextRb else nextState, nextIp
    let finalState, finalIp = processor 0L 0L
    finalState, finalIp, memory

let parse (mem: int[]) value mode =
    if mode = 0 then mem.[value] else value

let ops = Map.ofList [
    99L, (fun ip rb _ _ _ _ _ -> // halt
        ip, rb, Halted)

    1L, (fun ip rb v1 v2 set _ _ -> // Add
        v1() + v2() |> set 3L
        ip + 4L, rb, Running)

    2L, (fun ip rb v1 v2 set _ _ -> // Mul
        v1() * v2() |> set 3L
        ip + 4L, rb, Running)

    3L, (fun ip rb _ _ set read _-> // Read or block
        let (canRead, value) = read ()
        if canRead then
            set 1L value
            ip + 2L, rb, Running
        else
            ip, rb, Blocked)

    4L, (fun ip rb v1 _ _ _ write -> // Write
       write <| v1()
       ip + 2L, rb, Running)

    5L, (fun ip rb v1 v2 _ _ _ -> // jump if greater
        let nextIp = if v1() > 0L then v2() else ip + 3L
        nextIp, rb, Running)

    6L, (fun ip rb v1 v2 _ _ _ -> // jump if equal
        let nextIp = if v1() = 0L then v2() else ip + 3L
        nextIp, rb, Running)

    7L, (fun ip rb v1 v2 set _ _ -> // set if less
        set 3L <| if v1() < v2() then 1L else 0L
        ip + 4L, rb, Running)

    8L, (fun ip rb v1 v2 set _ _ -> // set if equal
        set 3L <| if v1() = v2() then 1L else 0L
        ip + 4L, rb, Running)

    9L, (fun ip rb v1 _ _ _ _ -> // alter relative base
        ip + 2L, rb + v1(), Running)
    ]

let read (queue: Queue<int64>) = 
    fun () -> if queue.Count > 0 then true, queue.Dequeue () else false, 0L


let part1 () =

    let input = Queue<int64>([1L])
    let output = Queue<int64>()
    let memory = Dictionary<int64, int64> (Array.indexed startMemory |> Array.map (fun (k, v) -> int64 k, v) |> dict)
    intcodeRun ops memory (read input) (output.Enqueue) |> ignore
    output.Dequeue () |> string

let part2 () =
    
    let input = Queue<int64>([2L])
    let output = Queue<int64>()
    let memory = Dictionary<int64, int64> (Array.indexed startMemory |> Array.map (fun (k, v) -> int64 k, v) |> dict)
    intcodeRun ops memory (read input) (output.Enqueue) |> ignore
    output.Dequeue () |> string