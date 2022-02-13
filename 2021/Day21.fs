module Day21

open Common
open System

let processed = readEmbedded "day21"

let init () =
    processed |> Array.length |> ignore

let starts = processed |> Array.map (fun s -> int s["Player 1 starting position: ".Length..])

let part1 () =
    let d100x3 d = 
        if d < 98 then
            d + d+1 + d+2, d+3
        else
            if d = 98 then 98+99+100, 1
            else if d = 99 then 99+100+1, 2
            else 100+1+2, 3

    let rec play r d p1 p2 p1s p2s =
        let p1roll, d = d100x3 d
        let p1 = let v = p1 + p1roll % 10 in if v > 10 then v - 10 else v
        if p1s + p1 >= 1000 then
            p2s * (r+3)
        else
            let p2roll, d = d100x3 d
            let p2 = let v = p2 + p2roll % 10 in if v > 10 then v - 10 else v
            if p2s + p2 >= 1000 then
                (p1s + p1) * (r+6)
            else
                play (r+6) d p1 p2 (p1s + p1) (p2s + p2)
    play 0 1 starts[0] starts[1] 0 0

let part2 () =

    // memoize dice, p1pos, p2pos, p1score, p2score to win counts for each
    let dict = System.Collections.Generic.Dictionary<int * int * int * int * int * int, int64 * int64>()

    let rec play idx acc p1 p2 p1s p2s = 
        if dict.ContainsKey((idx, acc, p1, p2, p1s, p2s)) then
            dict[(idx, acc, p1, p2, p1s, p2s)]
        else
            let res = 
                if p1s >= 21 then 1L, 0L
                else if p2s >= 21 then 0L, 1L
                else if idx <> 2 && idx <> 5 then
                    let (q1p1, q1p2) = play (idx + 1) (acc + 1) p1 p2 p1s p2s
                    let (q2p1, q2p2) = play (idx + 1) (acc + 2) p1 p2 p1s p2s
                    let (q3p1, q3p2) = play (idx + 1) (acc + 3) p1 p2 p1s p2s
                    q1p1 + q2p1 + q3p1, q1p2 + q2p2 + q3p2
                else
                    let p = if idx < 3 then p1 else p2
                    let cap v = if v > 10 then v - 10 else v
                    let s1 = cap (p + acc + 1)
                    let s2 = cap (p + acc + 2)
                    let s3 = cap (p + acc + 3)
                    let nd = (idx + 1) % 6
                    if idx < 3 then
                        let (q1p1, q1p2) = play nd 0 s1 p2 (p1s + s1) p2s
                        let (q2p1, q2p2) = play nd 0 s2 p2 (p1s + s2) p2s
                        let (q3p1, q3p2) = play nd 0 s3 p2 (p1s + s3) p2s
                        q1p1 + q2p1 + q3p1, q1p2 + q2p2 + q3p2
                    else
                        let (q1p1, q1p2) = play nd 0 p1 s1 p1s (p2s + s1)
                        let (q2p1, q2p2) = play nd 0 p1 s2 p1s (p2s + s2)
                        let (q3p1, q3p2) = play nd 0 p1 s3 p1s (p2s + s3)
                        q1p1 + q2p1 + q3p1, q1p2 + q2p2 + q3p2
            dict.Add((idx, acc, p1, p2, p1s, p2s), res)
            res

    let p1, p2 = 
        play 0 0 starts[0] starts[1] 0 0
    if p1 > p2 then p1 else p2
