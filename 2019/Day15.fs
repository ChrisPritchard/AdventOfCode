module Day15

open Common
open System.IO
open System.Collections.Generic

// type specification : this allows the system to be switched from int to int64 to bigint or float if necessary

type T = int64
type TQueue = Queue<T>
let stot (o: string) = int64 o
let itot (o: int) = int64 o

let t1m, t0, t1, t2, t3, t4, t100, t1000, t10000, t100000 = 
    itot -1, itot 0, itot 1, itot 2, itot 3, itot 4, 
    itot 100, itot 1000, itot 10000, itot 100000

let input = (File.ReadAllText ("./inputs/day15.txt")).Split ',' |> Array.map stot

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

// individual parts

let part1 () =    
    
    let mem =
        Array.indexed input 
        |> Array.map (fun (k, v) -> itot k, v) 
        |> dict 
        |> Dictionary<T, T>
    
    intcodeRun ops t0 t0 mem (read inputStream) (outputStream.Enqueue) |> ignore

    Seq.chunkBySize 3 outputStream
    |> Seq.map Seq.toArray
    |> Seq.filter (fun a -> a.[2] = t2)
    |> Seq.length

let part2 () =

    let mem =
        Array.indexed input 
        |> Array.map (fun (k, v) -> itot k, v) 
        |> dict 
        |> Dictionary<T, T>

    mem.[t0] <- t2
    
    let rec looper ip rb mem =
        let state, ip, rb, mem = intcodeRun ops ip rb mem (read inputStream) (outputStream.Enqueue)
        
        let blocks, score =
            Seq.chunkBySize 3 outputStream
            |> Seq.map Seq.toArray
            |> Seq.fold (fun (blocks, score) a -> 
                if a.[0] = t1m && a.[1] = t0 then blocks, int a.[2]
                else if a.[2] = t2 then blocks + 1, score
                else blocks, score) (0, 0)

        if blocks = 0 then score
        else if state = Halted then 
            outputStream.Clear ()
            looper t0 t0 mem
        else
            inputStream.Enqueue t0
            looper ip rb mem
            
    looper t0 t0 mem
    