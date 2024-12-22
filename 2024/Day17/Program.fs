let input = System.IO.File.ReadAllLines "input.txt"

let mutable reg_a = System.UInt64.Parse(input[0][12..])
let mutable reg_b = System.UInt64.Parse(input[1][12..])
let mutable reg_c = System.UInt64.Parse(input[2][12..])

let program = (input[4][9..]).Split(',') |> Array.map int

let combo =
    function
    | n when n <= 3 -> uint64 n
    | 4 -> reg_a
    | 5 -> reg_b
    | 6 -> reg_c
    | n -> failwithf "invalid combo operand: %d" n

let mutable ip = 0

let exec opcode operand =
    let mutable output = None

    match opcode with
    | 0 (* adv *) -> reg_a <- reg_a >>> int (combo operand)
    | 1 (* bxl *) -> reg_b <- reg_b ^^^ uint64 operand
    | 2 (* bst *) -> reg_b <- combo operand % 8UL
    | 3 (* jnz *) ->
        if reg_a <> 0UL then
            ip <- operand
    | 4 (* bxc *) -> reg_b <- reg_b ^^^ reg_c
    | 5 (* out *) -> output <- Some(combo operand % 8UL)
    | 6 (* bdv *) -> reg_b <- reg_a >>> int (combo operand)
    | 7 (* cdv *) -> reg_c <- reg_a >>> int (combo operand)
    | n -> failwithf "unknown opcode: %d" n

    if opcode <> 3 || reg_a = 0UL then
        ip <- ip + 2

    output

let mutable result = ""

while ip < program.Length do
    match exec program[ip] program[ip + 1] with
    | Some output when result = "" -> result <- output.ToString()
    | Some output -> result <- sprintf "%s,%d" result output
    | _ -> ()

printfn "Part 1: %s" result

reg_a <- 10000UL
reg_b <- 0UL
reg_c <- 0UL

printfn ""
printfn "let mutable a = %d" reg_a
printfn "let mutable b = %d" reg_a
printfn "let mutable c = %d" reg_a
printfn ""

ip <- 0

let combo_print =
    function
    | n when n <= 3 -> n.ToString()
    | 4 -> "a"
    | 5 -> "b"
    | 6 -> "c"
    | n -> failwithf "invalid combo operand: %d" n

while ip < program.Length do
    match program[ip] with
    | 0 -> printfn "a <- a >>> %s" (combo_print program[ip + 1])
    | 1 -> printfn "b <- b ^^^ %d" program[ip + 1]

    | 2 -> printfn "b <- %s %% 8" (combo_print program[ip + 1])
    | 3 ->
        if reg_a <> 0UL then
            ip <- program[ip + 1]
    | 4 -> printfn "b <- b ^^^ c"
    | 5 (* out *) -> printfn "printf \"%%d\" (%s %% 8)" (combo_print program[ip + 1])
    | 6 (* bdv *) -> printfn "b <- a >>> %s" (combo_print program[ip + 1])
    | 7 (* cdv *) -> printfn "c <- a >>> %s" (combo_print program[ip + 1])
    | _ -> ()

    if program[ip] <> 3 || reg_a = 0UL then
        ip <- ip + 2
