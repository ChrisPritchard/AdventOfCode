module Day09

open System.IO
open System.Collections.Generic


type T = int64
type TQueue = Queue<T>
let stot (o: string) = int64 o
let itot (o: int) = int64 o

let input = (File.ReadAllText ("./inputs/day09.txt")).Split ',' |> Array.map stot
//let input = "109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99" |> split "," |> Array.map stot
    
let t0, t1, t2, t3, t4, t100, t1000, t10000, t100000 = 
    itot 0, itot 1, itot 2, itot 3, itot 4, 
    itot 100, itot 1000, itot 10000, itot 100000

let read (queue: TQueue) = 
    fun () -> if queue.Count > 0 then true, queue.Dequeue () else false, t0
    
type State = Running | Halted | Blocked

let intcodeRun opcodes memoryArray read write =
    let memory = 
        Array.indexed memoryArray 
        |> Array.map (fun (k, v) -> itot k, v) 
        |> dict 
        |> Dictionary<T, T>
    let parseOp code = 
        let op = code % t100
        op, [|code % t1000 / t100; code % t10000 / t1000; code % t100000 / t10000|]
    let rec processor ip rb =
        let opcode, modes = parseOp memory.[ip]
        let v1 () = 
            let v = memory.[ip + t1] 
            if modes.[0] = t0 then memory.[v] 
            elif modes.[0] = t2 then memory.[rb + v]
            else v
        let v2 () = 
            let v = memory.[ip + t2] 
            if modes.[1] = t0 then memory.[v] 
            elif modes.[1] = t2 then memory.[rb + v]
            else v
        let set i v = 
            let o = memory.[ip + i]
            let sidx = if modes.[int i - 1] = t2 then rb + o else o
            memory.[sidx] <- v
        let op = Map.find opcode opcodes
        let nextIp, nextRb, nextState = op ip rb v1 v2 set read write
        if nextState = Running then processor nextIp nextRb else nextState, nextIp
    let finalState, finalIp = processor t0 t0
    finalState, finalIp, memory

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
    
let part1 () =

    let inputStream = TQueue([itot 1])
    let outputStream = TQueue()
    intcodeRun ops input (read inputStream) (outputStream.Enqueue) |> ignore
    outputStream.Dequeue () |> string

let part2 () =
    
    let inputStream = TQueue([itot 2])
    let outputStream = TQueue()
    intcodeRun ops input (read inputStream) (outputStream.Enqueue) |> ignore
    outputStream.Dequeue () |> string