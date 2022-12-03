module Day02

open Common
open System

let rock, paper, scissors = 1, 2, 3
let loss, draw, win = 0, 3, 6

let part1 () =
    readEmbeddedRaw "day02"
    |> Seq.sumBy (function
        | "A X" -> rock + draw // rock rock, draw
        | "A Y" -> paper + win // rock paper, win
        | "A Z" -> scissors + loss // rock scissors, loss
        | "B X" -> rock + loss // paper rock, loss
        | "B Y" -> paper + draw // paper paper, draw
        | "B Z" -> scissors + win // paper scissors, win
        | "C X" -> rock + win // scissors rock, win
        | "C Y" -> paper + loss // scissors paper, loss
        | "C Z" -> scissors + draw // scissors scissors, draw
        | _ -> failwith "shouldnt happen")

let part2 () =
    readEmbeddedRaw "day02"
    |> Seq.sumBy (function
        | "A X" -> scissors + loss // rock scissors, lose
        | "A Y" -> rock + draw // rock rock, draw
        | "A Z" -> paper + win // rock paper, win
        | "B X" -> rock + loss // paper rock, lose
        | "B Y" -> paper + draw // paper paper, draw
        | "B Z" -> scissors + win // paper scissors, win
        | "C X" -> paper + loss // scissors paper, lose
        | "C Y" -> scissors + draw // scissors scissors, draw
        | "C Z" -> rock + win // scissors rock, win
        | _ -> failwith "shouldnt happen")

