module Day18

open Common
open System

let processed = readEmbedded "day18"

let init () =
    processed |> Array.length |> ignore

let add a b =
    let mutable res = ("[" + a + "," + b + "]").ToCharArray() |> Array.map string

    let explodeAt index = 
        let left = int res[index + 1]
        let right = int res[index + 3]

        let mutable j = index - 1
        let mutable found = false
        while j >= 0 && not found do
            if Char.IsDigit (res[j][0]) then
                res[j] <- string (int res[j] + left)
                found <- true
            j <- j - 1
        j <- index + 5
        found <- false
        while j < res.Length && not found do
            if Char.IsDigit (res[j][0]) then
                res[j] <- string (int res[j] + right)
                found <- true
            j <- j + 1

        res <- Array.concat [| res[0..index-1] ; [|"0"|] ; res[index+5..] |]

    let splitAt index = 
        let value = float res[index]
        let (left, right) = int (floor (value / 2.)), int (ceil (value / 2.))
        res <- Array.concat [| res[0..index-1] ; [|"[";string left;",";string right;"]"|] ; res[index+1..] |]

    let mutable reduced = true
    while reduced do
        reduced <- false

        // test for explodes
        let mutable level = 0
        let mutable i = 0
        while i < res.Length && not reduced do
            match res[i] with
            | "[" when level < 4 ->
                level <- level + 1
            | "[" ->
                explodeAt i
                reduced <- true
            | "]" ->
                level <- level - 1
            | _ -> 
                ()
            i <- i + 1

        // test for splits
        i <- 0
        while i < res.Length && not reduced do
            if Char.IsDigit (res[i][0]) && int res[i] > 9 then
                splitAt i
                reduced <- true
            i <- i + 1
            
    res |> String.concat ""

let rec magnitude start (n: string) = 
    let res = n.ToCharArray() |> Array.map string
    let mutable i = start
    let mutable left = -1
    let mutable right = -1
    let mutable finished = false
    while i < res.Length && not finished do
        match res[i] with
        | "[" -> 
            let v, ni = magnitude (i + 1) n
            if left = -1 then left <- v
            else right <- v
            i <- ni - 1
        | "]" ->
            finished <- true
        | c when Char.IsDigit c[0] ->
            if left = -1 then left <- int c
            else right <- int c
        | _ -> ()
        i <- i + 1
    3 * left + 2 * right, i

let part1 () =
    processed |> Array.reduce add |> magnitude 1 |> fst

let part2 () =
    processed 
    |> Array.collect (fun a -> 
        processed 
        |> Array.except [|a|] 
        |> Array.map (fun b -> add a b |> magnitude 1 |> fst)) 
    |> Array.max