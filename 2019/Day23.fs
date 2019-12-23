module Day23

open Common
open System.IO
open System.Collections.Generic

let input = File.ReadAllText "./inputs/day23.txt" |> split ","

type Computer (address) =
    let io = Intcode.IO.create ()
    let mutable ip, rb, mem = 0L, 0L, Intcode.memFrom input

    do
        io.write address

    member _.Address = address

    member _.run messages =
        if Array.isEmpty messages then
            io.write -1L
        else
            for (x, y) in messages do
                io.write x
                io.write y
        let _, oip, orb, omem = Intcode.run ip rb mem io
        ip <- oip
        rb <- orb
        mem <- omem
        let sent = io.output |> Seq.toArray |> Array.windowed 3 |> Array.map (fun a -> a.[0], (a.[1], a.[2]))
        io.clear ()
        sent

let part1 () =

    let computers = Array.init 50 (int64 >> Computer)
    let messages = Array.init 50 (fun i -> int64 i, List<int64 * int64>()) |> Map.ofArray

    let rec runAll () =
        let mutable result = None
        for computer in computers do
            let received = messages.[computer.Address] |> Seq.toArray
            messages.[computer.Address].Clear() |> ignore
            let sent = computer.run received
            for (address, (x, y)) in sent do
                if address = 255L then result <- Some y
                if Map.containsKey address messages then 
                    messages.[address].Add (x, y)
        match result with
        | Some n -> n
        | _ -> runAll ()

    runAll () |> string

let part2 () =

    0