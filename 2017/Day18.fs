﻿(*
--- Day 18: Duet ---

You discover a tablet containing some strange assembly code labeled simply "Duet". Rather than bother the sound card with it, you decide to run the code yourself. Unfortunately, you don't see any documentation, so you're left to figure out what the instructions mean on your own.

It seems like the assembly is meant to operate on a set of registers that are each named with a single letter and that can each hold a single integer. You suppose each register should start with a value of 0.

There aren't that many instructions, so it shouldn't be hard to figure out what they do. Here's what you determine:

    snd X plays a sound with a frequency equal to the value of X.
    set X Y sets register X to the value of Y.
    add X Y increases register X by the value of Y.
    mul X Y sets register X to the result of multiplying the value contained in register X by the value of Y.
    mod X Y sets register X to the remainder of dividing the value contained in register X by the value of Y (that is, it sets X to the result of X modulo Y).
    rcv X recovers the frequency of the last sound played, but only when the value of X is not zero. (If it is zero, the command does nothing.)
    jgz X Y jumps with an offset of the value of Y, but only if the value of X is greater than zero. (An offset of 2 skips the next instruction, an offset of -1 jumps to the previous instruction, and so on.)

Many of the instructions can take either a register (a single letter) or a number. The value of a register is the integer it contains; the value of a number is that number.

After each jump instruction, the program continues with the instruction to which the jump jumped. After any other instruction, the program continues with the next instruction. Continuing (or jumping) off either end of the program terminates it.

For example:

set a 1
add a 2
mul a a
mod a 5
snd a
set a 0
rcv a
jgz a -1
set a 1
jgz a -2

    The first four instructions set a to 1, add 2 to it, square it, and then set it to itself modulo 5, resulting in a value of 4.
    Then, a sound with frequency 4 (the value of a) is played.
    After that, a is set to 0, causing the subsequent rcv and jgz instructions to both be skipped (rcv because a is 0, and jgz because a is not greater than 0).
    Finally, a is set to 1, causing the next jgz instruction to activate, jumping back two instructions to another jump, which jumps again to the rcv, which ultimately triggers the recover operation.

At the time the recover operation is executed, the frequency of the last sound played is 4.

What is the value of the recovered frequency (the value of the most recently played sound) the first time a rcv instruction is executed with a non-zero value?
*)

module Day18

open Common

let input = System.IO.File.ReadAllLines "./inputs/day18.txt"

let regVal reg registers =
    Map.tryFind reg registers |> Option.defaultValue 0L

let regOrVal (text: string) registers =
    if System.Char.IsLetter text.[0] then regVal text.[0] registers else int64 text

let soundReg = '@'
let recvReg = '&'

let runInstruction index (instructions: string []) registers =
    let elems = split " " instructions.[index]
    match elems.[0] with
    | "snd" -> 
        let reg = soundReg
        let newVal = regOrVal elems.[1] registers
        index + 1, Map.add reg newVal registers
    | "set" -> 
        let reg = elems.[1].[0]
        let newVal = regOrVal elems.[2] registers
        index + 1, Map.add reg newVal registers
    | "add" -> 
        let reg = elems.[1].[0]
        let newVal = regVal reg registers + regOrVal elems.[2] registers
        index + 1, Map.add reg newVal registers
    | "mul" -> 
        let reg = elems.[1].[0]
        let newVal = regVal reg registers * regOrVal elems.[2] registers
        index + 1, Map.add reg newVal registers
    | "mod" -> 
        let reg = elems.[1].[0]
        let newVal = regVal reg registers % regOrVal elems.[2] registers
        index + 1, Map.add reg newVal registers
    | "rcv" -> 
        let regVal = regOrVal elems.[1] registers
        if regVal = 0L then
            index + 1, registers
        else
            index + 1, Map.add recvReg regVal registers
    | "jgz" -> 
        let regVal = regOrVal elems.[1] registers
        let jump = regOrVal elems.[2] registers
        if regVal <= 0L then
            index + 1, registers
        else
            index + int jump, registers
    | _ -> failwith "unrecognised instruction"

let part1 () =
    
    (*
        // loop one
0 :        set i 31        
1 :        set a 1     
2 :    mul p 17        p is 0 so 0
3 :    jgz p p         no jump
4 :        mul a 2         
5 :        add i -1
6 :        jgz i -2        a becomes 2^31 = 2147483648    
7 :    add a -1        then a = 2147483647
        
        // loop two
8 :    set i 127
9 :    set p 952
10:        mul p 8505      8096760 on first loop   3631823828010 on second loop
11:        mod p a         no change               428980933
12:        mul p 129749    1050546513240           55659847075817
13:        add p 12345     1050546525585           55659847088162
14:        mod p a         427022202               1365925216
15:        set b p
16:        mod b 10000     2202                    5216
17:        snd b
18:    add i -1
19:    jgz i -9

20:    jgz a 3
21:        rcv b                infinite loop here if b > 0
22:        jgz b -1
23:            set f 0
24:            set i 126
25:            rcv a
26:                rcv b
27:                set p a
28:                mul p -1
29:                add p b
30:                jgz p 4
31:                snd a
32:                set a b
33:                jgz 1 3
34:                snd b
35:                set f 1
36:                add i -1
37:                jgz i -11
38:            snd a
39:            jgz f -16
40:        jgz a -19*)

    let mutable index = 0
    let mutable registers = Map.empty
    while index >= 0 && index < input.Length do
        let newIndex, newRegisters = runInstruction index input registers
        index <- newIndex
        registers <- newRegisters
        if index = 21 && registers.['b'] > 0L then
            index <- -1
    registers.[recvReg]

let part2 () =
    0