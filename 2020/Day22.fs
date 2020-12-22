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
        |> Array.map int)
    |> fun a -> a.[0], a.[1]

let part1 () =
    let player1, player2 = processed ()
    let rec playGame p1 p2 =
        match p1, p2 with
        | o, [||] | [||], o ->
            o |> Array.rev |> Array.indexed |> Array.sumBy (fun (i, c) -> (i+1) * c)
        | p1, p2 ->
            let p1c, p2c = p1.[0], p2.[0]
            let p1, p2 =
                if p1c > p2c then
                    Array.append p1.[1..] [|p1c; p2c|], p2.[1..]
                else
                    p1.[1..], Array.append p2.[1..] [|p2c; p1c|]
            playGame p1 p2
    playGame player1 player2

type Victory =
    | Player1 of int []
    | Player2 of int []

let part2 () =
    let player1, player2 = processed ()
    let rec playGame p1 p2 acc =
        if Set.contains (p1, p2) acc then
            Player1 p1
        else
            let acc = Set.add (p1, p2) acc
            match p1, p2 with
            | o, [||] -> Player1 o
            | [||], o -> Player2 o
            | p1, p2 ->
                let p1c, p2c = p1.[0], p2.[0]
                let p1, p2 = 
                    if p1c > p1.[1..].Length || p2c > p2.[1..].Length then
                        if p1c > p2c then
                            Array.append p1.[1..] [|p1c; p2c|], p2.[1..]
                        else
                            p1.[1..], Array.append p2.[1..] [|p2c; p1c|]
                    else
                        match playGame p1.[1..p1c] p2.[1..p2c] Set.empty with
                        | Player1 _ -> Array.append p1.[1..] [|p1c; p2c|], p2.[1..]
                        | Player2 _ -> p1.[1..], Array.append p2.[1..] [|p2c; p1c|]
                playGame p1 p2 acc
    match playGame player1 player2 Set.empty with
    | Player1 o | Player2 o ->
        o |> Array.rev |> Array.indexed |> Array.sumBy (fun (i, c) -> (i+1) * c)