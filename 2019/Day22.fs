module Day22

open Common
open System
open System.IO
open System.Collections.Generic

let input = File.ReadAllLines "./inputs/day22.txt"

let part1 () =

    let applyIncrement increment deck =
        let length = Array.length deck
        let result = Array.create length 0
        for i = 0 to length - 1 do
            if i = 0 then result.[0] <- deck.[0]
            else 
                let index = (i * increment) % length
                result.[index] <- deck.[i]
        result
    
    let rec applyCut cut deck =
        if cut >= 0 then
            let taken = Array.take cut deck
            let start = Array.skip cut deck
            Array.append start taken
        else
            let point = Array.length deck + cut
            applyCut point deck
    
    let apply deck =
        function
        | "deal into new stack" -> Array.rev deck
        | s when s.StartsWith "deal with increment " ->
            let increment = s.Substring "deal with increment ".Length |> int
            applyIncrement increment deck
        | s when s.StartsWith "cut " ->
            let cut = s.Substring "cut ".Length |> int
            applyCut cut deck
        | s -> failwithf "'%s' didn't match a handler" s

    let res = ([|0..10006|], input) ||> Array.fold apply
    Array.findIndex ((=) 2019) res

(*
For part 2, after experimenting for a day, I gave up and checked Reddit. This one was a struggle for many people,
as it uses some advanced math concepts that I at least, certainly do not know.

In lieu of trying to learn advanced calculus and linear algebra, I settled for providing an implementation of the algorithm
demonstrated in this reddit comment: https://www.reddit.com/r/adventofcode/comments/ee0rqi/2019_day_22_solutions/fbnifwk/

The reversing of the index was something I was working on (though I didn't know about mod inverse and so failed), but the 
linear/congruent/whatsits are beyond me.
*)

let part2 () =

    let reverseStack totalLength index =
        totalLength - 1L - index

    let reverseCut cut totalLength index =
        (index + cut + totalLength) % totalLength

    let modInverse a m =
        let rec egcd a b =
            if a = 0L then (b, 0L, 1L)
            else 
                let g, y, x = egcd (b % a) a
                (g, x - (b / a) * y, y)

        let g, x, _ = egcd a m
        if g <> 1L then failwith "modular inverse does not exist"
        else x % m

    let reverseIncrement increment totalLength index =
        modInverse increment totalLength * index % totalLength

    let reverseApply totalLength index =
        function
        | "deal into new stack" -> reverseStack totalLength index
        | s when s.StartsWith "deal with increment " ->
            let increment = s.Substring "deal with increment ".Length |> int64
            reverseIncrement increment totalLength index
        | s when s.StartsWith "cut " ->
            let cut = s.Substring "cut ".Length |> int64
            reverseCut cut totalLength index
        | s -> failwithf "'%s' didn't match a handler" s
        
    let D = 119315717514047L
    let n = 101741582076661L

    let reverse index = 
        (index, Array.rev input) ||> Array.fold (reverseApply D)

    let X = 2020L
    let Y = reverse X
    let Z = reverse Y
    let A = (Y - Z) * (modInverse ((X - Y) + D) D) % D
    let B = (Y - (A*X)) % D

    let pow x y z = int64 (float x ** float y) % z
    ((pow A n D)*X + ((pow A n D)-1L) * (modInverse (A-1L) D) * B) % D