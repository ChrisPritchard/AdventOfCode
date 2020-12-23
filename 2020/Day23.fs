module Day23

open Common
open System.IO

let input = File.ReadAllLines "./inputs/day23.txt"

let processed () =
    input.[0] |> Seq.map (fun c -> int c - 48) |> Seq.toArray

let part1 () =
    let start = processed ()
    let min, max = Array.min start, Array.max start
    let rec round index (current: int []) c =
        if c = 100 then current
        else
            let cup = current.[index]
            let nextThree = current.[index+1..] |> Array.truncate 3
            let nextThree, current = 
                let short = 3 - nextThree.Length
                if short = 0 then
                    nextThree, Array.append current.[0..index] current.[index+4..]
                else
                    Array.append nextThree current.[0..short-1], current.[short..index]
            let rec findDest n =
                if n < min then 
                    findDest max 
                elif Array.contains n nextThree then
                    findDest (n - 1)
                else
                    n
            let destCup = findDest (cup - 1)
            let dest = Array.findIndex (fun o -> o = destCup) current
            let current = Array.concat [|current.[0..dest]; nextThree; current.[dest+1..]|]
            let offset = Array.findIndex (fun o -> o = cup) current - index
            let current = Array.append current.[offset..] current.[0..offset-1]
            let index = if index = current.Length - 1 then 0 else index + 1
            round index current (c + 1)
    let final = round 0 start 0
    let offset = Array.findIndex (fun o -> o = 1) final
    Array.append final.[offset+1..] final.[0..offset-1]
    |> Array.map string |> String.concat ""

let part2 () =
    0