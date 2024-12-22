let input = System.IO.File.ReadAllLines "input.txt"

let mutable reg_a = System.Int64.Parse(input[0][12..])
let mutable reg_b = System.Int64.Parse(input[1][12..])
let mutable reg_c = System.Int64.Parse(input[2][12..])

let program = (input[4][9..]).Split(',') |> Array.map int

let combo =
    function
    | n when n <= 3 -> int64 n
    | 4 -> reg_a
    | 5 -> reg_b
    | 6 -> reg_c
    | n -> failwithf "invalid combo operand: %d" n

let mutable ip = 0

let pown2 n = int64 (System.Math.Pow(2., float n))

let mutable result = ""

let exec opcode operand =
    match opcode with
    | 0 (* adv *) -> reg_a <- reg_a / pown2 (combo operand)
    | 1 (* bxl *) -> reg_b <- reg_b ^^^ operand
    | 2 (* bst *) -> reg_b <- combo operand % 8L
    | 3 (* jnz *) ->
        if reg_a <> 0 then
            ip <- operand
    | 4 (* bxc *) -> reg_b <- reg_b ^^^ reg_c
    | 5 (* out *) ->
        let value = combo operand % 8L

        if result = "" then
            result <- value.ToString()
        else
            result <- result + "," + value.ToString()
    | 6 (* bdv *) -> reg_b <- reg_a / pown2 (combo operand)
    | 7 (* cdv *) -> reg_c <- reg_a / pown2 (combo operand)
    | n -> failwithf "unknown opcode: %d" n

    if opcode <> 3 || reg_a = 0 then
        ip <- ip + 2

while ip < program.Length do
    exec program[ip] program[ip + 1]

printfn "Part 1: %s" result
