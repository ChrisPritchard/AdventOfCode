module Day02

open Common
open System

let rock, paper, scissors = 1, 2, 3
let loss, draw, win = 0, 3, 6

let part1 () =
    (0, readEmbeddedRaw "day02")
    ||> 
        Seq.fold (fun s (v: string) ->
            match v with
            | "A X" -> s + rock + draw // rock rock, draw
            | "A Y" -> s + paper + win // rock paper, win
            | "A Z" -> s + scissors + loss // rock scissors, loss
            | "B X" -> s + rock + loss // paper rock, loss
            | "B Y" -> s + paper + draw // paper paper, draw
            | "B Z" -> s + scissors + win // paper scissors, win
            | "C X" -> s + rock + win // scissors rock, win
            | "C Y" -> s + paper + loss // scissors paper, loss
            | "C Z" -> s + scissors + draw // scissors scissors, draw
            | _ -> failwith "shouldnt happen"
            )

let part2 () =
    (0, readEmbeddedRaw "day02")
    ||> 
        Seq.fold (fun s (v: string) ->
            match v with
            | "A X" -> s + scissors + loss // rock scissors, lose
            | "A Y" -> s + rock + draw // rock rock, draw
            | "A Z" -> s + paper + win // rock paper, win
            | "B X" -> s + rock + loss // paper rock, lose
            | "B Y" -> s + paper + draw // paper paper, draw
            | "B Z" -> s + scissors + win // paper scissors, win
            | "C X" -> s + paper + loss // scissors paper, lose
            | "C Y" -> s + scissors + draw // scissors scissors, draw
            | "C Z" -> s + rock + win // scissors rock, win
            | _ -> failwith "shouldnt happen"
            )

