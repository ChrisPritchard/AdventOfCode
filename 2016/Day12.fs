(*
--- Day 12: Leonardo's Monorail ---

You finally reach the top floor of this building: a garden with a slanted glass ceiling. Looks like there are no more stars to be had.

While sitting on a nearby bench amidst some tiger lilies, you manage to decrypt some of the files you extracted from the servers downstairs.

According to these documents, Easter Bunny HQ isn't just this building - it's a collection of buildings in the nearby area. They're all connected by a local monorail, and there's another building not far from here! Unfortunately, being night, the monorail is currently not operating.

You remotely connect to the monorail control systems and discover that the boot sequence expects a password. The password-checking logic (your puzzle input) is easy to extract, but the code it uses is strange: it's assembunny code designed for the new computer you just assembled. You'll have to execute the code and get the password.

The assembunny code you've extracted operates on four registers (a, b, c, and d) that start at 0 and can hold any integer. However, it seems to make use of only a few instructions:

    cpy x y copies x (either an integer or the value of a register) into register y.
    inc x increases the value of register x by one.
    dec x decreases the value of register x by one.
    jnz x y jumps to an instruction y away (positive means forward; negative means backward), but only if x is not zero.

The jnz instruction moves relative to itself: an offset of -1 would continue at the previous instruction, while an offset of 2 would skip over the next instruction.

For example:

cpy 41 a
inc a
inc a
dec a
jnz a 2
dec a

The above code would set register a to 41, increase its value by 2, decrease its value by 1, and then skip the last dec a (because a is not zero, so the jnz a 2 skips it), leaving register a at 42. When you move past the last instruction, the program halts.

After executing the assembunny code in your puzzle input, what value is left in register a?
*)   

(*
--- Part Two ---

As you head down the fire escape to the monorail, you notice it didn't start; register c needs to be initialized to the position of the ignition key.

If you instead initialize register c to be 1, what value is now left in register a?
*)

module Day12

open Common

//let input = 
//    [|
//        "cpy 41 a"
//        "inc a"
//        "inc a"
//        "dec a"
//        "jnz a 2"
//        "dec a"
//    |]
    
let input = 
    [|
        "cpy 1 a"
        "cpy 1 b"
        "cpy 26 d"
        "jnz c 2"
        "jnz 1 5"
        "cpy 7 c"
        "inc d"
        "dec c"
        "jnz c -2"
        "cpy a c"
        "inc a"
        "dec b"
        "jnz b -2"
        "cpy c b"
        "dec d"
        "jnz d -6"
        "cpy 19 c"
        "cpy 11 d"
        "inc a"
        "dec d"
        "jnz d -2"
        "dec c"
        "jnz c -5"
    |]

type Instruction =
    | CopyValue of int64 * char
    | CopyRegister of char * char
    | Increment of char
    | Decrement of char
    | Jump of int
    | JumpNotZero of char * int

let parseInstruction text = 
    let segments = split " " text
    match segments.[0] with
    | "inc" -> Increment segments.[1].[0]
    | "dec" -> Decrement segments.[1].[0]
    | "jnz" -> 
        if System.Char.IsLetter segments.[1].[0] then
            JumpNotZero (segments.[1].[0], int segments.[2])
        else
            Jump (if int segments.[1] = 0 then 1 else int segments.[2])
    | "cpy" | _ -> 
        if System.Char.IsLetter segments.[1].[0] then
            CopyRegister (segments.[1].[0], segments.[2].[0])
        else
            CopyValue (int64 segments.[1], segments.[2].[0])

let getRegister r registers = 
    Map.tryFind r registers |> Option.defaultValue 0L

let instructions = input |> Array.map parseInstruction

let rec runInstruction registers i =
    if i >= instructions.Length || i < 0 then
        getRegister 'a' registers
    else
        match instructions.[i] with
        | Increment r -> 
            let next = registers |> getRegister r |> fun e -> Map.add r (e + 1L) registers
            runInstruction next (i + 1)
        | Decrement r ->
            let next = registers |> getRegister r |> fun e -> Map.add r (e - 1L) registers
            runInstruction next (i + 1)
        | CopyValue (v, r) ->
            let next = Map.add r v registers
            runInstruction next (i + 1)
        | CopyRegister (ra, r) ->
            let next = Map.add r (getRegister ra registers) registers
            runInstruction next (i + 1)
        | Jump v ->
            runInstruction registers (i + v)
        | JumpNotZero (r, v) ->
            if  getRegister r registers = 0L then
                runInstruction registers (i + 1)    
            else
                runInstruction registers (i + v)

let part1 () =
    runInstruction Map.empty 0

let part2 () =
    runInstruction (Map.empty.Add ('c', 1L)) 0