open System.IO
open System.Threading.Tasks
open System.Threading

let setReg index registers rval = 
    registers |> List.mapi (fun i v -> if i = index then rval else v)

let addr a b c (registers: int list) =
    registers.[a] + registers.[b] |> setReg c registers

let addi a b c (registers: int list) =
    registers.[a] + b |> setReg c registers

let mulr a b c (registers: int list) =
    registers.[a] * registers.[b] |> setReg c registers

let muli a b c (registers: int list) =
    registers.[a] * b |> setReg c registers

let banr a b c (registers: int list) =
    registers.[a] &&& registers.[b] |> setReg c registers

let bani a b c (registers: int list) =
    registers.[a] &&& b |> setReg c registers

let borr a b c (registers: int list) =
    registers.[a] ||| registers.[b] |> setReg c registers

let bori a b c (registers: int list) =
    registers.[a] ||| b |> setReg c registers

let setr a _ c (registers: int list) =
    registers.[a] |> setReg c registers

let seti a _ c (registers: int list) =
    a |> setReg c registers

let gtir a b c (registers: int list) =
    (if a > registers.[b] then 1 else 0) |> setReg c registers

let gtri a b c (registers: int list) =
    (if registers.[a] > b then 1 else 0) |> setReg c registers

let gtrr a b c (registers: int list) =
    (if registers.[a] > registers.[b] then 1 else 0) |> setReg c registers

let eqir a b c (registers: int list) =
    (if a = registers.[b] then 1 else 0) |> setReg c registers

let eqri a b c (registers: int list) =
    (if registers.[a] = b then 1 else 0) |> setReg c registers

let eqrr a b c (registers: int list) =
    (if registers.[a] = registers.[b] then 1 else 0) |> setReg c registers

let opMap = 
    [
        "addr", addr; "addi", addi; "mulr", mulr; "muli", muli
        "banr", banr; "bani", bani; "borr", borr; "bori", bori
        "setr", setr; "seti", seti
        "gtir", gtir; "gtri", gtri; "gtrr", gtrr; 
        "eqir", eqir; "eqri", eqri; "eqrr", eqrr
    ] 
    |> Map.ofList

let parseInput (lines: string []) =
    let parseInt = System.Int32.Parse
    let pc = parseInt <| lines.[0].Split([|' '|]).[1]
    let prog = 
        lines.[1..lines.Length-1] 
        |> Array.mapi (fun i line ->
            let parts = line.Split([|' '|])
            i, (parts.[0], parseInt parts.[1], parseInt parts.[2], parseInt parts.[3]))
        |> Map.ofArray
    pc, prog

let runProgram pc prog (start: int list) =
    let mutable registers = start
    let mutable halted = false
    while not halted do
        if registers.[pc] = 28 then
            halted <- true
        else
            let opCode, a, b, c = Map.find registers.[pc] prog
            let op = opMap.[opCode]
            registers <- op a b c registers
            if registers.[pc] >= prog.Count-1 then halted <- true
            else registers <- setReg pc registers (registers.[pc] + 1)
    registers.[5]

[<EntryPoint>]
let main _ =
    
    let input = File.ReadAllLines "input.txt"
    let pc, prog = parseInput input

    let reg5 = runProgram pc prog [0;0;0;0;0;0]
    printfn "part 1: %i" reg5

    0
