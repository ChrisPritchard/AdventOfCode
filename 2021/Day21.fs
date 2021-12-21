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
    // dice can fall in 27 different ways
    // a mere eight roles will result 282 billion possibilities, so arrays are out of the question
    // math might make it easier:
        // each player will likely win in two or three rounds
        // in the 27 dice options, there are only 7 values: 3 - 9
        // for a given starting position, this can result in 7 scores after round one, e.g.
            // if p1 starts at 4, their score can be 7,8,9,10,1,2,3 after round 1
            // after round 2 this becomes:
                // from 7: 17,8,9,10,11,12,13
                // from 8: 9,10,11,12,13,14,15
                // from 9: 11,12,13,14,15,16,17
                // from 10: 13,14,15,16,17,18,19
                // from 1: 5,6,7,8,9,10,11
                // from 2: 7,8,9,10,11,12,3
                // from 3: 9,10,11,12,13,4,5
            // after round 3...
    
    0