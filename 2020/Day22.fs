module Day22

open Common
open System.IO

let input = File.ReadAllText "./inputs/day22.txt"

let processed () =
    input 
    |> splitOn (newline + newline) 
    |> Array.map (fun player -> 
        player 
        |> splitOn newline 
        |> Array.tail 
        |> String.concat "")
    |> fun a -> a.[0], a.[1]

let part1 () =
    let player1, player2 = processed ()
    let rec playGame p1 p2 =
        match p1, p2 with
        | o, "" | "", o ->
            o |> Seq.rev |> Seq.indexed |> Seq.sumBy (fun (i, c) -> (i+1) * (int c - 41))
        | p1, p2 ->
            let p1c, p2c = string p1.[0], string p2.[0]
            let p1, p2 =
                if p1c > p2c then
                    p1 + p1c + p2c, p2.[1..]
                else
                    p1.[1..], p2 + p2c + p1c
            playGame p1 p2
    playGame player1 player2

    // remove head of both
    // append to sub of winner

let part2 () =
    0