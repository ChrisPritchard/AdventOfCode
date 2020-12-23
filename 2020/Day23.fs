module Day23

open System.IO
open System.Collections.Generic

let input = File.ReadAllLines "./inputs/day23.txt"

let processed () =
    let numbers = input.[0] |> Seq.map (fun c -> int c - 48) |> Seq.toArray
    let next = Dictionary<int, int>()
    for i in [0..numbers.Length-1] do
        let right = if i = numbers.Length - 1 then 0 else i + 1
        next.Add (numbers.[i], numbers.[right])
    numbers.[0], Array.min numbers, Array.max numbers, next

let part1 () =
    let start, min, max, next = processed ()
    let rec findDest n n1 n2 n3 =
        if n < min then 
            findDest max n1 n2 n3
        elif n1 = n || n2 = n || n3 = n then
            findDest (n - 1) n1 n2 n3
        else
            n
    let rec round current count =
        if count = 100 then
            let rec compiler acc n =
                if n = 1 then acc else compiler (acc + string n) next.[n]
            compiler "" next.[1]
        else
            let n1 = next.[current]
            let n2 = next.[n1]
            let n3 = next.[n2]
            next.[current] <- next.[n3]
            let dest = findDest (current - 1) n1 n2 n3
            next.[n3] <- next.[dest]
            next.[dest] <- n1
            round next.[current] (count + 1)
    round start 0

let part2 () =
    0