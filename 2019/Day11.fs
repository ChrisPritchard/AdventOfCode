﻿module Day11

open Common
open System.IO
open System.Collections.Generic

// type specification : this allows the system to be switched from int to int64 to bigint or float if necessary

type T = int64
type TQueue = Queue<T>
let stot (o: string) = int64 o
let itot (o: int) = int64 o

let t0, t1, t2, t3, t4, t100, t1000, t10000, t100000 = 
    itot 0, itot 1, itot 2, itot 3, itot 4, 
    itot 100, itot 1000, itot 10000, itot 100000

let input = (File.ReadAllText ("./inputs/day11.txt")).Split ',' |> Array.map stot

// generic intcode vm code

let read (queue: TQueue) = 
    fun () -> if queue.Count > 0 then true, queue.Dequeue () else false, t0
    
type State = Running | Halted | Blocked

let intcodeRun opcodes ip rb (memory: Dictionary<T, T>) read write =
    let parseOp code = 
        let op = code % t100
        op, [|code % t1000 / t100; code % t10000 / t1000; code % t100000 / t10000|]
    let rec processor ip rb =
        let opcode, modes = parseOp memory.[ip]
        let value i = if memory.ContainsKey i then memory.[i] else t0
        let v1 () = 
            let v = memory.[ip + t1] 
            if modes.[0] = t0 then value v
            elif modes.[0] = t2 then value (rb + v)
            else v
        let v2 () = 
            let v = memory.[ip + t2] 
            if modes.[1] = t0 then value v
            elif modes.[1] = t2 then value (rb + v)
            else v
        let set i v = 
            let o = memory.[ip + i]
            let sidx = if modes.[int i - 1] = t2 then rb + o else o
            memory.[sidx] <- v
        let op = Map.find opcode opcodes
        let nextIp, nextRb, nextState = op ip rb v1 v2 set read write
        if nextState = Running then processor nextIp nextRb else nextState, nextIp, nextRb
    let finalState, finalIp, finalRb = processor ip rb
    finalState, finalIp, finalRb, memory

let ops = Map.ofList [
    itot 99, (fun ip rb _ _ _ _ _ -> // halt
        ip, rb, Halted)

    itot 1, (fun ip rb v1 v2 set _ _ -> // Add
        v1() + v2() |> set t3
        ip + t4, rb, Running)

    itot 2, (fun ip rb v1 v2 set _ _ -> // Mul
        v1() * v2() |> set t3
        ip + t4, rb, Running)

    itot 3, (fun ip rb _ _ set read _-> // Read or block
        let (canRead, value) = read ()
        if canRead then
            set t1 value
            ip + t2, rb, Running
        else
            ip, rb, Blocked)

    itot 4, (fun ip rb v1 _ _ _ write -> // Write
       write <| v1()
       ip + t2, rb, Running)

    itot 5, (fun ip rb v1 v2 _ _ _ -> // jump if greater
        let nextIp = if v1() > t0 then v2() else ip + t3
        nextIp, rb, Running)

    itot 6, (fun ip rb v1 v2 _ _ _ -> // jump if equal
        let nextIp = if v1() = t0 then v2() else ip + t3
        nextIp, rb, Running)

    itot 7, (fun ip rb v1 v2 set _ _ -> // set if less
        set t3 <| if v1() < v2() then t1 else t0
        ip + t4, rb, Running)

    itot 8, (fun ip rb v1 v2 set _ _ -> // set if equal
        set t3 <| if v1() = v2() then t1 else t0
        ip + t4, rb, Running)

    itot 9, (fun ip rb v1 _ _ _ _ -> // alter relative base
        ip + t2, rb + v1(), Running)
    ]

// painter code for day 11

let inputStream = TQueue()
let outputStream = TQueue()
    
let turnLeft (dx, dy) =
    if dx = 1 then 0, -1
    elif dx = -1 then 0, 1
    elif dy = 1 then 1, 0
    else -1, 0

let turnRight (dx, dy) =
    if dx = 1 then 0, 1
    elif dx = -1 then 0, -1
    elif dy = 1 then -1, 0
    else 1, 0

let rec painter (map: Map<int * int, T>) (x, y) dir (_, ip, rb, mem) =
    let currentTile = Map.tryFind (x, y) map |> Option.defaultValue t0
    inputStream.Enqueue(currentTile)

    let (state, ip, rb, mem) = intcodeRun ops ip rb mem (read inputStream) (outputStream.Enqueue)

    let map = 
        if outputStream.Count > 0 then 
            Map.add (x, y) (outputStream.Dequeue()) map
        else
            failwith "expected colour"
    let (dx, dy) = 
        if outputStream.Count > 0 then 
            if (outputStream.Dequeue()) = t0 then
                turnLeft dir
            else
                turnRight dir
        else
            failwith "expected turn instruction"

    let pos = (x + dx, y + dy)
    if state = Halted 
    then 
        map 
    else
        painter map pos (dx, dy) (state, ip, rb, mem)

// individual parts

let part1 () =    
    
    let mem =
        Array.indexed input 
        |> Array.map (fun (k, v) -> itot k, v) 
        |> dict 
        |> Dictionary<T, T>
    
    let final = painter Map.empty (0, 0) (0, -1) (Running, t0, t0, mem)
    final.Count

let part2 () =
    
    let mem =
        Array.indexed input 
        |> Array.map (fun (k, v) -> itot k, v) 
        |> dict 
        |> Dictionary<T, T>
    
    let final = painter (Map.empty.Add ((0, 0), t1)) (0, 0) (0, -1) (Running, t0, t0, mem)
    
    asString <| seq {
        yield '\n'
        for y = 0 to 6 do
            for x = 0 to 40 do
                yield 
                    Map.tryFind (x, y) final |> Option.defaultValue t0 |> function n when n = t0 -> ' ' | _ -> '#'
            yield '\n'
    }