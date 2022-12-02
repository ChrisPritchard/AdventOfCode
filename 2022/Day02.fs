module Day02

open Common
open System

let part1 () =
    (0, readEmbeddedRaw "day02")
    ||> 
        Seq.fold (fun s (v: string) ->
            match v with
            | "A X" -> s + 1 + 3 // rock rock, draw
            | "A Y" -> s + 2 + 6 // rock paper, win
            | "A Z" -> s + 3 + 0 // rock scissors, loss
            | "B X" -> s + 1 + 0 // paper rock, loss
            | "B Y" -> s + 2 + 3 // paper paper, draw
            | "B Z" -> s + 3 + 6 // paper scissors, win
            | "C X" -> s + 1 + 6 // scissors rock, win
            | "C Y" -> s + 2 + 0 // scissors paper, loss
            | "C Z" -> s + 3 + 3 // scissors scissors, draw
            | _ -> s // shouldnt happen 
            )

let part2 () =
    (0, readEmbeddedRaw "day02")
    ||> 
        Seq.fold (fun s (v: string) ->
            match v with
            | "A X" -> s + 2 + 0 // rock scissors, lose
            | "A Y" -> s + 1 + 3 // rock rock, draw
            | "A Z" -> s + 3 + 6 // rock paper, win
            | "B X" -> s + 1 + 0 // paper rock, lose
            | "B Y" -> s + 3 + 3 // paper paper, draw
            | "B Z" -> s + 2 + 6 // paper scissors, win
            | "C X" -> s + 3 + 0 // scissors paper, lose
            | "C Y" -> s + 2 + 3 // scissors scissors, draw
            | "C Z" -> s + 1 + 6 // scissors rock, win
            | _ -> s // shouldnt happen 
            )

