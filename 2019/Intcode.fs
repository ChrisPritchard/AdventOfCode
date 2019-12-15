module Intcode

open System.Collections.Generic
    
type State = Running | Halted | Blocked

type IO = {
    input: Queue<int64>
    output: Queue<int64>
}
with 
    static member create() = 
        { input = Queue<int64>(); output = Queue<int64>() }
    member o.read() =
        if o.output.Count > 0 then true, o.output.Dequeue () else false, 0L
    member o.write n =
        o.input.Enqueue n
    member o.clear() = 
        o.input.Clear()
        o.output.Clear()
        
let memFrom (array: string[]) =
    Array.indexed array 
    |> Array.map (fun (k, v) -> int64 k, int64 v) 
    |> dict 
    |> Dictionary<int64, int64>

let private opcodes = Map.ofList [
    99L, (fun ip rb _ _ _ _ -> // halt
        ip, rb, Halted)

    1L, (fun ip rb v1 v2 set _ -> // Add
        v1() + v2() |> set 3L
        ip + 4L, rb, Running)

    2L, (fun ip rb v1 v2 set _ -> // Mul
        v1() * v2() |> set 3L
        ip + 4L, rb, Running)

    3L, (fun ip rb _ _ set (io: IO) -> // Read or block
        let (canRead, value) = 
            if io.input.Count > 0 then true, io.input.Dequeue () 
            else false, 0L
        if canRead then
            set 1L value
            ip + 2L, rb, Running
        else
            ip, rb, Blocked)

    4L, (fun ip rb v1 _ _ (io: IO) -> // Write
       io.output.Enqueue <| v1()
       ip + 2L, rb, Running)

    5L, (fun ip rb v1 v2 _ _ -> // jump if greater
        let nextIp = if v1() > 0L then v2() else ip + 3L
        nextIp, rb, Running)

    6L, (fun ip rb v1 v2 _ _ -> // jump if equal
        let nextIp = if v1() = 0L then v2() else ip + 3L
        nextIp, rb, Running)

    7L, (fun ip rb v1 v2 set _ -> // set if less
        set 3L <| if v1() < v2() then 1L else 0L
        ip + 4L, rb, Running)

    8L, (fun ip rb v1 v2 set _ -> // set if equal
        set 3L <| if v1() = v2() then 1L else 0L
        ip + 4L, rb, Running)

    9L, (fun ip rb v1 _ _ _ -> // alter relative base
        ip + 2L, rb + v1(), Running)
    ]
    
let run ip rb (memory: Dictionary<int64, int64>) (io: IO) =

    let parseOp code = 
        let op = code % 100L
        op, [|code % 1000L / 100L; code % 10000L / 1000L; code % 100000L / 10000L|]

    let rec processor ip rb =
        let opcode, modes = parseOp memory.[ip]
        let value i = if memory.ContainsKey i then memory.[i] else 0L

        let v1 () = 
            let v = memory.[ip + 1L] 
            if modes.[0] = 0L then value v
            elif modes.[0] = 2L then value (rb + v)
            else v
        let v2 () = 
            let v = memory.[ip + 2L] 
            if modes.[1] = 0L then value v
            elif modes.[1] = 2L then value (rb + v)
            else v
        let set i v = 
            let o = memory.[ip + i]
            let sidx = if modes.[int i - 1] = 2L then rb + o else o
            memory.[sidx] <- v

        let op = Map.find opcode opcodes
        let nextIp, nextRb, nextState = op ip rb v1 v2 set io

        if nextState = Running then 
            processor nextIp nextRb 
        else 
            nextState, nextIp, nextRb

    let finalState, finalIp, finalRb = processor ip rb
    finalState, finalIp, finalRb, memory