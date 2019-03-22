(*
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

(*
--- Part Two ---

What do you get if you multiply together the values of one chip in each of outputs 0, 1, and 2?
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

let instructions = input |> Array.map instruction |> Array.toList

let rec processor bots outs target list =
    match list with
    | [] -> 
        let next = instructions |> List.filter (function | Give _ -> true | _ -> false)
        if bots |> Map.toList |> List.exists (fun (_, c) -> List.length c > 1) then
            processor bots outs target next
        else
            -1, outs
    | (Start (v, b))::rest -> 
        let bot = Map.tryFind b bots |> Option.defaultValue []
        let bots = Map.add b (v::bot) bots
        processor bots outs target rest
    | (Give (b, low, high))::rest ->
        let chips = Map.tryFind b bots |> Option.defaultValue [] |> List.sort
        if chips.Length <> 2 then
            processor bots outs target rest
        else
            match target with
            | Some t when t = chips -> b, outs
            | _ ->
                let bots, outs = 
                    match low with 
                    | Bot b -> 
                        let bot = Map.tryFind b bots |> Option.defaultValue []
                        Map.add b (chips.[0]::bot) bots, outs
                    | Output o -> 
                        let output = Map.tryFind o outs |> Option.defaultValue []
                        bots, Map.add o (chips.[0]::output) outs
                let bots, outs = 
                    match high with 
                    | Bot b -> 
                        let bot = Map.tryFind b bots |> Option.defaultValue []
                        Map.add b (chips.[1]::bot) bots, outs
                    | Output o -> 
                        let output = Map.tryFind o outs |> Option.defaultValue []
                        bots, Map.add o (chips.[1]::output) outs
                let finalBots = Map.add b [] bots
                processor finalBots outs target rest

let part1 () =
    processor Map.empty Map.empty (Some [17;61]) instructions |> fst

let part2 () =
    let outs = processor Map.empty Map.empty None instructions |> snd
    outs |> Map.toSeq |> Seq.truncate 3 |> Seq.map (snd >> Seq.head) |> Seq.reduce (*)