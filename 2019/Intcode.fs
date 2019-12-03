module Intcode

let run opcodes startIp startMem =
    let memory = Array.copy startMem
    let rec processor ip =
        let opcode = memory.[ip]
        if opcode = 99 || not (Map.containsKey opcode opcodes) then ()
        else
            let (op, inc) = opcodes.[opcode]
            op ip memory
            processor (ip + inc)
    processor startIp
    memory
    
let ops = Map.ofList [
    1, ((fun pIndex (mem: int[]) ->
        mem.[mem.[pIndex + 3]] <- mem.[mem.[pIndex + 1]] + mem.[mem.[pIndex + 2]]), 4)
    2, ((fun pIndex (mem: int[]) ->
        mem.[mem.[pIndex + 3]] <- mem.[mem.[pIndex + 1]] * mem.[mem.[pIndex + 2]]), 4)
]

let drun = run ops 0