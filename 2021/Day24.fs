module Day24

open Common
open System

(*
input is 14 blocks of broadly the same instructions with some different variables. e.g. the last digit:

inp w   

mul x 0
add x z
mod x 26 // x = z % 26

div z 26 // 1 in some indexes, e.g. dont divide

add x 0  // add first constant
eql x w  // if equal to digit then 1 else 0
eql x 0  // invert: if equal then 0 else 1

mul y 0
add y 25
mul y x  // y = 25 or 0 based on x
add y 1  // y = 26 or 1 based on x
mul z y  // z = z * 26 if x is 1

mul y 0  
add y w  // y = digit
add y 6  // y = digit + constant
mul y x  // if x = 0 then y = 0, and wont be added, else
add z y  // if x = 1 then y will be (digit + constant)

these translate to:

let mutable z = 0
for digit in source do
    let x = if (z % 26) + [constant1] = digit then 0 else 1
    if index = 3, 7, 8, 10,11,12,13
        z <- z / 26
    if x = 0 then z <- z * 26
    if x = 1 then z <- z + ([constant2] + digit)

constants are 
constant1: 14 12 11 -4 10 10 15 -9 -9 12 -15 -7 -10 0
constant2: 7 4 8 1 5 14 12 10 5 7 6 8 4 6

in order to succeed, the final result must be 0

more simply:

let res z w (d, c, o) =
    if z % 26 + c = w then 
        z / d 
    else 
        ((z / d) * 26) + o + w

// help for this solution came from https://www.ericburden.work/blog/2022/01/05/advent-of-code-2021-day-24/

the input can be viewed as a stack, with the divisor 1 and 26 being the open and close statements. 
on my input this can be vidualised as:

1, 14, 7
    1, 12, 4
        1, 11, 8
        26, -4, 1
        1, 10, 5
            1, 10, 14
                1, 15, 12
                26, -9, 10
            26, -9, 5
            1, 12, 7
            26, -15, 6
        26, -7, 8
    26, -10, 4
26, 0, 6

with the ongoing z value viewed as a base-26 number. 
if z % 26 + c = w for opcode 1 (dont divide) this does nothing, else it adds the offset and w
if z % 26 + c = w for opcode 26 (divide) this will remove the added value (as it will always be less than 26)
ideally on opcode 1 we want to add a val, and on opcode 26 we want to remove it

1, 14, 7 with 26, 0, 6:
will always add 7 + w
assuming by last line 7 + w is all thats left, 
(7 + w0) + 0 = w13
2, 9

1, 12, 4 with 26, -10, 4
(4 + w1) - 10 = w12
9, 3

1, 11, 8 with 26, -4, 1
(8 + w2) - 4 = w3
5, 9

1, 10, 5 with 26, -7, 8
(5 + w4) - 7 = w11
9, 4

1, 10, 14 with 26, -9, 5
(14 + w5) - 9 = w8
4, 9

1, 15, 12 with 26, -9, 10
(12 + w6) - 9 = w7
6, 9

1, 12, 7 with 26, -15, 6
(7 + w9) - 15 = w10
9, 1

so possibly, 29599469991439?

*)

let processed = readEmbedded "day24" |> Array.map (split " ")

let init () =
    processed |> Array.length |> ignore

let config = 
    [|
        1, 14, 7
        1, 12, 4
        1, 11, 8
        26, -4, 1
        1, 10, 5
        1, 10, 14
        1, 15, 12
        26, -9, 10
        26, -9, 5
        1, 12, 7
        26, -15, 6
        26, -7, 8
        26, -10, 4
        26, 0, 6
    |]

let res z w (d, c, o) =
    if z % 26 + c = w then 
        z / d 
    else 
        ((z / d) * 26) + o + w

let processLine (line: string[]) (read: char -> int) (write: char -> int -> unit) (input: unit -> int) =
    if line[0] = "inp" then write (line[1][0]) (input())
    else
        let target = line[1][0]
        let source = if Char.IsLetter (line[2][0]) then (read (line[2][0])) else int (line[2])
        let res = 
            match line[0] with
            | "add" -> read target + source
            | "mul" -> read target * source
            | "div" -> read target / source
            | "mod" -> read target % source
            | "eql" | _ -> if read target = source then 1 else 0
        write target res

let validateNumber (n: int64) = 
    let s = string n
    if s.Contains "0" then -1L
    else
        let mutable candidate = s |> Seq.toList |> List.map (string >> int)
        let input () = 
            match candidate with 
            | head::rest ->
                candidate <- rest
                head
            | _ -> failwith "input exhausted"
        let mem = Array.create 4 0
        let idx = function 'w' -> 0 | 'x' -> 1 | 'y' -> 2 | _ -> 3
        let read a = mem[idx a]
        let write a v = mem[idx a] <- v
        for line in processed do processLine line read write input
        read 'z'

let part1 () =

    validateNumber 29599469991439L

let part2 () =
    
    0