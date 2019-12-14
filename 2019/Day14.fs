module Day14

open Common
open System.IO

let input = File.ReadAllLines "./inputs/day14.txt"

let transforms =
    input
    |> Array.map (fun s -> 
        s
        |> split " ,=>"
        |> fun a ->
            a.[a.Length - 1], 
                (int64 a.[a.Length - 2], 
                    a 
                    |> Array.truncate (a.Length - 2) 
                    |> Array.chunkBySize 2 
                    |> Array.map (fun b -> b.[1], int64 b.[0])))
    |> Map.ofArray

let adder multiplier (ore, required) (other, needed) =
    if other = "ORE" then ore + (needed * multiplier), required
    else ore, (other, needed * multiplier)::required

let replacer (ore, spare, required) (needed, amount) =
    let trueAmount = amount - (Map.tryFind needed spare |> Option.defaultValue 0L)
    if trueAmount < 0L then
        (ore, Map.add needed (abs trueAmount) spare, required)
    elif trueAmount = 0L then 
        (ore, Map.remove needed spare, required)
    else
        let (amountMade, requirements) = transforms.[needed]
        let multiplier = ceil (float trueAmount / float amountMade) |> int64
        let spare = Map.add needed ((multiplier * amountMade) - trueAmount) spare
        let ore, required = 
            ((ore, required), requirements) 
            ||> Array.fold (adder multiplier)
        ore, spare, required

let rec expander ore spare (required: (string * int64) list) =
    let ore, spare, required = 
        ((ore, spare, []), required)
        ||> List.fold replacer
    if List.length required = 0 then ore
    else
        expander ore spare required

let part1 () =    
    
    let start = snd transforms.["FUEL"]
    expander 0L Map.empty (List.ofArray start) |> string

let part2 () =

    let fuel n = 
        let start = snd transforms.["FUEL"] |> Array.map (fun (s, on) -> s, n * on)
        expander 0L Map.empty (List.ofArray start)

    let target = 1000000000000L

    let rec find min max =
        if max - min < 50L then
            [max..(-1L)..min] |> List.find (fun m -> fuel m <= target)
        else
            let mid = ((max - min) / 2L) + min
            let ore = fuel mid
            if ore < target then
                find mid max
            else //if ore > target then
                find min mid
    
    let for0 = fuel 1L
    let min = target / for0
    let max = 2L * min

    find min max |> string