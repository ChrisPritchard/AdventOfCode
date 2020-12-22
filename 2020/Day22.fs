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
    |> fun a -> List.ofArray a.[0], List.ofArray a.[1]

let part1 () =
    let player1, player2 = processed ()
    let rec playGame p1p p1s p2p p2s =
        match p1p, p2p with
        | p1, [] ->
            (p1 @ p1s) |> List.rev |> List.indexed |> List.sumBy (fun (i, c) -> (i+1) * c)
        | [], p2 ->
            (p2 @ p2s) |> List.rev |> List.indexed |> List.sumBy (fun (i, c) -> (i+1) * c)
        | p1c::p1rem, p2c::p2rem ->
            let p1s, p2s =
                if p1c > p2c then
                    p1s @ [p1c;p2c], p2s
                else
                    p1s, p2s @ [p2c;p1c]
            let p1p, p1s =
                match p1rem with
                | [] -> p1s, []
                | _ -> p1rem, p1s
            let p2p, p2s = 
                match p2rem with
                | [] -> p2s, []
                | _ -> p2rem, p2s
            playGame p1p p1s p2p p2s
    playGame player1 [] player2 []

    // remove head of both
    // append to sub of winner

let part2 () =
    0