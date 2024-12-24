let input = System.IO.File.ReadAllLines "input.txt"

let input_reg_a = System.UInt64.Parse(input[0][12..])
let input_reg_b = System.UInt64.Parse(input[1][12..])
let input_reg_c = System.UInt64.Parse(input[2][12..])

let program = (input[4][9..]).Split(',') |> Array.map int

let run_program program start_a start_b start_c =

    let mutable reg_a = start_a
    let mutable reg_b = start_b
    let mutable reg_c = start_c

    let combo =
        function
        | n when n <= 3 -> uint64 n
        | 4 -> reg_a
        | 5 -> reg_b
        | 6 -> reg_c
        | n -> failwithf "invalid combo operand: %d" n

    let combo_print =
        function
        | n when n <= 3 -> n.ToString()
        | 4 -> "a"
        | 5 -> "b"
        | 6 -> "c"
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

    let mutable result = []

    while ip < Array.length program do
        match exec program[ip] program[ip + 1] with
        | Some output -> result <- int output :: result
        | _ -> ()

    List.rev result |> List.toArray

let result = run_program program input_reg_a input_reg_b input_reg_c
let as_string = Array.map (fun n -> n.ToString()) result |> String.concat ","

printfn "Part 1: %s" as_string

(*

by printing out the instructions as fsharp code, can determine the program is:

instructions:
b <- a % 8
b <- b ^^^ 4
c <- a >>> b
b <- b ^^^ c
b <- b ^^^ 4
printf "%d" (b % 8)
a <- a >>> 3
b <- a % 8
b <- b ^^^ 4
c <- a >>> b
b <- b ^^^ c
b <- b ^^^ 4
printf "%d" (b % 8)
a <- a >>> 3
etc...

however followed a different solution, a simple approach done here: 
https://github.com/ading2210/advent-of-code-solutions/blob/main/2024/day17/day17.py

*)

let mutable calculated = 2UL <<< 44 // 8 ^ 15
let mutable power = 41
let mutable to_match = program[program.Length - 1 ..]

let mutable output = Array.empty

while output <> program do
    calculated <- calculated + (2UL <<< power)
    output <- run_program program calculated 0UL 0UL

    let to_compare = output[output.Length - to_match.Length ..]

    if to_compare = to_match then
        power <- max 0 (power - 3)
        to_match <- program[program.Length - (to_match.Length + 1) ..]

printfn "Part 2: %d" calculated
