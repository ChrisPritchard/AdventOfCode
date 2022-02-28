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
eql x w  // if equal to digit then 0 else 1
eql x 0  // invert

mul y 0
add y 25
mul y x
add y 1
mul z y  // if x = 1 then y will be 26 else 1 i.e. dont multiply

mul y 0  
add y w  
add y 6  
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
this means the final x must be 0 (in order for the constant, 6, not to be added)
z will be multiplied by 26 if x is 0, therefore z must also be 0 to end up as 0
however, as this is integer devision, z can be any value less than 26 in order to end up as 0 after division
finally, for z to be 0, z must equal the digit, which means it must be 1-9

going from other end, every constant1 greater than 9 will result in an x of 1,
meaning that the final result will be z + constant2 + the digit (w)

for index 0, the result will be 7 + the digit
for index 1, the result will be 7 + 4 + digit 1 + digit 2
for index 2 the result will be 7 + 4 + 8 + digit 1 + digit 2 + digit 3, or 19 + first three digits

for index 3 the result will be:
    if z > 26 then z = 1 else 0
    if z % 26 - 4 = digit then z = z * 26 (26 or 0, based on z initial val)
    else z = 1 + digit + (26 or 0, based on initial val
    
for index 4 5 and 6 the first constant is too high, so x will be 0
these also dont divide so z stays the same
meaning z will be z after index three, plus 31 (5 + 14 + 12) + the sum of the three digits



*)


let init () =
    () // not used


let part1 () =

    let constant1 = [|14; 12; 11; -4; 10; 10; 15; -9; -9; 12; -15; -7; -10; 0|]
    let constant2 = [| 7;  4;  8;  1;  5; 14; 12; 10;  5;  7;   6;  8;   4; 6|]
    let divIndexes = Set.ofArray [|3;7;8;10;11;12;13|]

    let res z w i =
        let x = if z % 26 + constant1[i] = w then 0 else 1
        let z = if Set.contains i divIndexes then z / 26 else z
        if x = 0 then z * 26 else z + constant2[i] + w

    let range () = Array.init 9 (id >> (+) 1)
    let firstThree = 
        range () 
            |> Array.collect (fun i0 -> 
                range () 
                |> Array.collect (fun i1 ->
                    range () 
                    |> Array.map (fun i2 -> i0, i1, i2)))
    
    for (i0, i1, i2) in firstThree do
        let result = 
            [|i0; i1; i2|] 
            |> Array.indexed 
            |> Array.fold (fun z (i, w) -> res z w i) 0
        let m = result = 19 + i0 + i1 + i2
        printfn "%b" m
        

let part2 () =
    
    0