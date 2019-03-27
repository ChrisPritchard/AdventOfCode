(*
--- Day 23: Safe Cracking ---

This is one of the top floors of the nicest tower in EBHQ. The Easter Bunny's private office is here, complete with a safe hidden behind a painting, and who wouldn't hide a star in a safe behind a painting?

The safe has a digital screen and keypad for code entry. A sticky note attached to the safe has a password hint on it: "eggs". The painting is of a large rabbit coloring some eggs. You see 7.

When you go to type the code, though, nothing appears on the display; instead, the keypad comes apart in your hands, apparently having been smashed. Behind it is some kind of socket - one that matches a connector in your prototype computer! You pull apart the smashed keypad and extract the logic circuit, plug it into your computer, and plug your computer into the safe.

Now, you just need to figure out what output the keypad would have sent to the safe. You extract the assembunny code from the logic chip (your puzzle input).

The code looks like it uses almost the same architecture and instruction set that the monorail computer used! You should be able to use the same assembunny interpreter for this as you did there, but with one new instruction:

tgl x toggles the instruction x away (pointing at instructions like jnz does: positive means forward; negative means backward):

    For one-argument instructions, inc becomes dec, and all other one-argument instructions become inc.
    For two-argument instructions, jnz becomes cpy, and all other two-instructions become jnz.
    The arguments of a toggled instruction are not affected.
    If an attempt is made to toggle an instruction outside the program, nothing happens.
    If toggling produces an invalid instruction (like cpy 1 2) and an attempt is later made to execute that instruction, skip it instead.
    If tgl toggles itself (for example, if a is 0, tgl a would target itself and become inc a), the resulting instruction is not executed until the next time it is reached.

For example, given this program:

cpy 2 a
tgl a
tgl a
tgl a
cpy 1 a
dec a
dec a

    cpy 2 a initializes register a to 2.
    The first tgl a toggles an instruction a (2) away from it, which changes the third tgl a into inc a.
    The second tgl a also modifies an instruction 2 away from it, which changes the cpy 1 a into jnz 1 a.
    The fourth line, which is now inc a, increments a to 3.
    Finally, the fifth line, which is now jnz 1 a, jumps a (3) instructions ahead, skipping the dec a instructions.

In this example, the final value in register a is 3.

The rest of the electronics seem to place the keypad entry (the number of eggs, 7) in register a, run the code, and then send the value left in register a to the safe.

What value should be sent to the safe?
*)

module Day23

open Common

let input = System.IO.File.ReadAllLines "Day23-input.txt"

type Ref = Value of int | Register of char

let refOf (s: string) =
    if System.Char.IsLetter s.[0] then Register s.[0] else Value (int s)

type Instruction =
    | Copy of Ref * Ref
    | Increment of Ref
    | Decrement of Ref
    | JumpNotZero of Ref * Ref
    | Toggle of Ref

let letter s = System.Char.IsLetter (Seq.head s)

let parseInstruction text = 
    let segments = split " " text
    match segments.[0] with
    | "tgl" -> Toggle (refOf segments.[1])
    | "inc" -> Increment (refOf segments.[1])
    | "dec" -> Decrement (refOf segments.[1])
    | "cpy" -> Copy (refOf segments.[1], refOf segments.[2])
    | "jnz" | _ -> JumpNotZero (refOf segments.[1], refOf segments.[2])

let getRegister r registers = 
    Map.tryFind r registers |> Option.defaultValue 0

let toggle instruction =
    match instruction with
    | Increment r -> Decrement r
    | Toggle r | Decrement r -> Increment r
    | JumpNotZero (r1, r2) -> Copy (r1, r2)
    | Copy (r1, r2) -> JumpNotZero (r1, r2)

let instructions = input |> Array.map parseInstruction

let rec runInstruction registers i =
    if i >= instructions.Length || i < 0 then
        getRegister 'a' registers
    else
        match instructions.[i] with
        | Toggle (Register r) ->
            let ir = i + int (getRegister r registers)
            if ir >= 0 && ir < instructions.Length then 
                instructions.[ir] <- toggle instructions.[ir]
            runInstruction registers (i + 1)
        | Increment (Register r) -> 
            let next = registers |> getRegister r |> fun e -> Map.add r (e + 1) registers
            runInstruction next (i + 1)
        | Decrement (Register r) ->
            let next = registers |> getRegister r |> fun e -> Map.add r (e - 1) registers
            runInstruction next (i + 1)
        | Copy ((Value v), (Register r)) ->
            let next = Map.add r v registers
            runInstruction next (i + 1)
        | Copy ((Register ra), (Register r)) ->
            let next = Map.add r (getRegister ra registers) registers
            runInstruction next (i + 1)
        | JumpNotZero ((Register r), (Value v)) ->
            let check = if getRegister r registers = 0 then 1 else v
            runInstruction registers (i + check)  
        | JumpNotZero ((Register r), (Register rv)) ->
            let check = if getRegister r registers = 0 then 1 else getRegister rv registers
            runInstruction registers (i + check)  
        | JumpNotZero ((Value v1), (Value v2)) ->
            let check = if v1 = 0 then 1 else v2
            runInstruction registers (i + check)  
        | JumpNotZero ((Value v), (Register rv)) ->
            let check = if v = 0 then 1 else getRegister rv registers
            runInstruction registers (i + check)  
        | _ -> 
            runInstruction registers (i + 1)

let part1 () =
    runInstruction (Map.empty.Add ('a', 7)) 0