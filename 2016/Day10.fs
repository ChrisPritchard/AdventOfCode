﻿(*
--- Day 10: Balance Bots ---

You come upon a factory in which many robots are zooming around handing small microchips to each other.

Upon closer examination, you notice that each bot only proceeds when it has two microchips, and once it does, it gives each one to a different bot or puts it in a marked "output" bin. Sometimes, bots take microchips from "input" bins, too.

Inspecting one of the microchips, it seems like they each contain a single number; the bots must use some logic to decide what to do with each chip. You access the local control computer and download the bots' instructions (your puzzle input).

Some of the instructions specify that a specific-valued microchip should be given to a specific bot; the rest of the instructions indicate what a given bot should do with its lower-value or higher-value chip.

For example, consider the following instructions:

value 5 goes to bot 2
bot 2 gives low to bot 1 and high to bot 0
value 3 goes to bot 1
bot 1 gives low to output 1 and high to bot 0
bot 0 gives low to output 2 and high to output 0
value 2 goes to bot 2

    Initially, bot 1 starts with a value-3 chip, and bot 2 starts with a value-2 chip and a value-5 chip.
    Because bot 2 has two microchips, it gives its lower one (2) to bot 1 and its higher one (5) to bot 0.
    Then, bot 1 has two microchips; it puts the value-2 chip in output 1 and gives the value-3 chip to bot 0.
    Finally, bot 0 has two microchips; it puts the 3 in output 2 and the 5 in output 0.

In the end, output bin 0 contains a value-5 microchip, output bin 1 contains a value-2 microchip, and output bin 2 contains a value-3 microchip. In this configuration, bot number 2 is responsible for comparing value-5 microchips with value-2 microchips.

Based on your instructions, what is the number of the bot that is responsible for comparing value-61 microchips with value-17 microchips?
*)

module Day10

open Common

let input = System.IO.File.ReadAllLines "Day10-input.txt"
//let input = 
//    [|
//        "value 5 goes to bot 2"
//        "bot 2 gives low to bot 1 and high to bot 0"
//        "value 3 goes to bot 1"
//        "bot 1 gives low to output 1 and high to bot 0"
//        "bot 0 gives low to output 2 and high to output 0"
//        "value 2 goes to bot 2"
//    |]

type Instruction =
    | Start of value:int * bot:int
    | Give of bot:int * low:Destination * high:Destination
and Destination = 
    | Bot of int | Output of int

let instruction (line: string) =
    if line.StartsWith "value" then
        let parts = splits ["value ";" goes to bot "] line
        Start (int parts.[0], int parts.[1])
    else
        let parts = splits [" gives low to ";" and high to "] line
        let bot = int (split " " parts.[0]).[1]
        let low = 
            match split " " parts.[1] with
            | [|"bot";n|] -> Bot (int n)
            | [|_;n|] -> Output (int n)
            | _ -> failwith "bad parse"
        let high = 
            match split " " parts.[2] with
            | [|"bot";n|] -> Bot (int n)
            | [|_;n|] -> Output (int n)
            | _ -> failwith "bad parse"
        Give (bot, low, high)

let part1 () =
    let instructions = input |> Array.map instruction |> Array.toList

    let bots = 
        instructions 
        |> List.choose (function Start (v,b) -> Some (b, v) | _ -> None)
        |> List.groupBy fst
        |> List.map (fun (k, v) -> k, v |> List.map snd |> List.sort)
        |> Map.ofList
    
    let rules =
        instructions 
        |> List.choose (function Give (b,low,high) -> Some (b, (low, high)) | _ -> None)
        |> Map.ofList
        
    let target = [17; 61]

    let pair dest n =
        match dest with
        | Bot b -> Some (b, n), None
        | Output o -> None, Some (o, n)
    
    let changes low high (destLow, destHigh) =
        let grouped = [destLow,low;destHigh,high] |> List.map (fun (d, n) -> pair d n)
        grouped |> List.choose fst, grouped |> List.choose snd

    let rec processor bots outs = 
        let bot = bots |> Map.findKey (fun _ c -> List.length c = 2) |> fun k -> k, Map.find k bots
        if snd bot = target then fst bot
        else
            let botChanges, outchanges = changes (snd bot).[0] (snd bot).[1] rules.[fst bot]
            let newBots = 
                (bots, botChanges) 
                ||> List.fold (fun map (b, c) -> 
                    match Map.tryFind b map with
                    | None -> Map.add b [c] map
                    | Some ce -> Map.add b (List.sort (c::ce)) map)
            let newOuts = 
                (outs, outchanges) 
                ||> List.fold (fun map (o, c) -> 
                    match Map.tryFind o map with
                    | None -> Map.add o [c] map
                    | Some ce -> Map.add o (List.sort (c::ce)) map)
            processor newBots newOuts

    processor bots Map.empty