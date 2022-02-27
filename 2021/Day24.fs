module Day24

open Common
open System

(*
input is 14 blocks of broadly the same instructions with some different variables:

inp w	inp w	inp w	inp w	inp w	inp w	inp w	inp w	inp w	inp w	inp w	inp w	inp w	inp w
mul x 0	mul x 0	mul x 0	mul x 0	mul x 0	mul x 0	mul x 0	mul x 0	mul x 0	mul x 0	mul x 0	mul x 0	mul x 0	mul x 0
add x z	add x z	add x z	add x z	add x z	add x z	add x z	add x z	add x z	add x z	add x z	add x z	add x z	add x z
mod x 26	mod x 26	mod x 26	mod x 26	mod x 26	mod x 26	mod x 26	mod x 26	mod x 26	mod x 26	mod x 26	mod x 26	mod x 26	mod x 26
div z 1	div z 1	div z 1	div z 26	div z 1	div z 1	div z 1	div z 26	div z 26	div z 1	div z 26	div z 26	div z 26	div z 26
add x 14	add x 12	add x 11	add x -4	add x 10	add x 10	add x 15	add x -9	add x -9	add x 12	add x -15	add x -7	add x -10	add x 0
eql x w	eql x w	eql x w	eql x w	eql x w	eql x w	eql x w	eql x w	eql x w	eql x w	eql x w	eql x w	eql x w	eql x w
eql x 0	eql x 0	eql x 0	eql x 0	eql x 0	eql x 0	eql x 0	eql x 0	eql x 0	eql x 0	eql x 0	eql x 0	eql x 0	eql x 0
mul y 0	mul y 0	mul y 0	mul y 0	mul y 0	mul y 0	mul y 0	mul y 0	mul y 0	mul y 0	mul y 0	mul y 0	mul y 0	mul y 0
add y 25	add y 25	add y 25	add y 25	add y 25	add y 25	add y 25	add y 25	add y 25	add y 25	add y 25	add y 25	add y 25	add y 25
mul y x	mul y x	mul y x	mul y x	mul y x	mul y x	mul y x	mul y x	mul y x	mul y x	mul y x	mul y x	mul y x	mul y x
add y 1	add y 1	add y 1	add y 1	add y 1	add y 1	add y 1	add y 1	add y 1	add y 1	add y 1	add y 1	add y 1	add y 1
mul z y	mul z y	mul z y	mul z y	mul z y	mul z y	mul z y	mul z y	mul z y	mul z y	mul z y	mul z y	mul z y	mul z y
mul y 0	mul y 0	mul y 0	mul y 0	mul y 0	mul y 0	mul y 0	mul y 0	mul y 0	mul y 0	mul y 0	mul y 0	mul y 0	mul y 0
add y w	add y w	add y w	add y w	add y w	add y w	add y w	add y w	add y w	add y w	add y w	add y w	add y w	add y w
add y 7	add y 4	add y 8	add y 1	add y 5	add y 14	add y 12	add y 10	add y 5	add y 7	add y 6	add y 8	add y 4	add y 6
mul y x	mul y x	mul y x	mul y x	mul y x	mul y x	mul y x	mul y x	mul y x	mul y x	mul y x	mul y x	mul y x	mul y x
add z y	add z y	add z y	add z y	add z y	add z y	add z y	add z y	add z y	add z y	add z y	add z y	add z y	add z y

this translates to:

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

*)


let init () =
    () // not used


let part1 () =

    let constant1 = [|14; 12; 11; -4; 10; 10; 15; -9; -9; 12; -15; -7; -10; 0|]
    let constant2 = [|7; 4; 8; 1; 5; 14; 12; 10; 5; 7; 6; 8; 4; 6|]
    let divIndexes = Set.ofArray [|3;7;8;10;11;12;13|]

    let validateNumber (n: int64) = 
        let s = string n
        if s.Contains "0" then false
        else
            let res = 
                s |> Seq.map int |> Seq.indexed |> Seq.fold (fun z (idx, w) ->
                    let x = if z % 26 + constant1[idx] = w then 0 else 1
                    let z = if Set.contains idx divIndexes then z / 26 else z
                    if x = 0 then z * 26 else z + constant2[idx] + w) 0
            res = 0


    let mutable num = 99999999999999L
    let mutable result = false
    while not result do
        result <- validateNumber num
        num <- num - 1L
    num + 1L
        

let part2 () =
    
    0