module Day18

open Common
open System

let processed = readEmbedded "day18"

let init () =
    processed |> Array.length |> ignore

let add a b =
    let mutable n = "[" + a + "," + b + "]"
    let mutable reduced = true

    while reduced do
        reduced <- false

        let mutable level = 0
        let mutable lastRegular = -1
        let mutable i = 0

        while i < n.Length && not reduced do
            match n[i] with
            | '[' when level < 4 -> 
                level <- level + 1

            | '[' when level = 4 -> 

                let (left, right, nextStart) =
                    if n[i+4] = ']' then asInt n[i+1], asInt n[i+3], 5
                    else if n[i+2] = ',' then asInt n[i+1], int n[i+3..i+4], 6
                    else if n[i+5] = ']' then int n[i+1..i+2], asInt n[i+4], 6
                    else int n[i+1..i+2], int n[i+4..i+5], 7

                let mutable prev = n[0..(i-1)]
                if lastRegular <> -1 then
                    let lastN = asInt n[lastRegular]
                    let newN = lastN + left
                    prev <- n[0..(lastRegular - 1)] + string newN + n[(lastRegular+1)..(i-1)]

                let mutable next = n[(i+nextStart)..]
                let mutable j = 0
                while j < next.Length do
                    if Char.IsDigit next[j] then
                        if Char.IsDigit next[j + 1] then
                            let nextN = int next[j..j+1]
                            let newN = nextN + right
                            next <- next[0..(j - 1)] + string newN + next[(j+2)..]
                        else
                            let nextN = asInt next[j]
                            let newN = nextN + right
                            next <- next[0..(j - 1)] + string newN + next[(j+1)..]
                        j <- next.Length
                    j <- j + 1

                n <- prev + "0" + next
                
                reduced <- true

            | ']' -> 
                level <- level - 1
            | c when Char.IsDigit c && lastRegular <> i - 1 -> 
                lastRegular <- i
            | c when Char.IsDigit c ->

                // split
                let number = float (asString [n[i-1];c])
                let left = int (floor (number / 2.))
                let right = int (ceil (number / 2.))

                if left < 0 || right < 0 then
                    failwith "unexpected split"

                n <- n[0..i-2] + "[" + string left + "," + string right + "]" + n[(i+1)..]
                reduced <- true

            | _ -> () // ignore others

            i <- i + 1
        
        printfn "%s\n" n
    n


let part1 () =
    processed |> Array.reduce add
    //add "[[[[4,3],4],4],[7,[[8,4],9]]]" "[1,1]"

let part2 () =
    0