module Intcode


let parseOp code = 
    let op = code % 100
    (op, code % 1000 / 100, code % 10000 / 1000, code % 100000 / 10000)

let run opcodes startIp startMem =
    let memory = Array.copy startMem
    let rec processor ip =
        let opcode, mode1, mode2, mode3 = parseOp memory.[ip]
        if opcode = 99 || not (Map.containsKey opcode opcodes) then ()
        else
            let op = opcodes.[opcode]
            let nextIp = op ip memory [|mode1; mode2; mode3|]
            processor nextIp
    processor startIp
    memory
    
//let ops = Map.ofList [
//    1, ((fun pIndex (mem: int[]) ->
//        mem.[mem.[pIndex + 3]] <- mem.[mem.[pIndex + 1]] + mem.[mem.[pIndex + 2]]), 4)
//    2, ((fun pIndex (mem: int[]) ->
//        mem.[mem.[pIndex + 3]] <- mem.[mem.[pIndex + 1]] * mem.[mem.[pIndex + 2]]), 4)
//]

//let drun = run ops 0