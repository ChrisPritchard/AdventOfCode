module Day06

open Common
open System

let processed = readEmbedded "day06" |> Array.head |> split "," |> Array.map int

let init () =
    processed |> Array.length |> ignore

let part1 () =
    let byTimes = Array.zeroCreate 9
    let initial = Array.countBy id processed |> Map.ofArray
    for i = 0 to 8 do
        if Map.containsKey i initial then
            byTimes[i] <- initial[i]
    
    for i = 1 to 80 do
        let newFish = byTimes[0]
        for j = 1 to 8 do
            byTimes[j-1] <- byTimes[j]
        byTimes[8] <- newFish
        byTimes[6] <- byTimes[6] + newFish

    Array.sum byTimes

let part2 () =
    let byTimes = Array.create 9 0L
    let initial = Array.countBy id processed |> Map.ofArray
    for i = 0 to 8 do
        if Map.containsKey i initial then
            byTimes[i] <- initial[i]
    
    for i = 1 to 256 do
        let newFish = byTimes[0]
        for j = 1 to 8 do
            byTimes[j-1] <- byTimes[j]
        byTimes[8] <- newFish
        byTimes[6] <- byTimes[6] + newFish

    Array.sum byTimes

(*
// original solution for part 1 - did NOT scale to part 2, and was slow

let part1 () =
    let folder pool _ = 
        let mutable newFish = 0
        for i = 0 to Array.length pool - 1 do
            if pool[i] = 0 then
                pool[i] <- 6
                newFish <- newFish + 1
            else
                pool[i] <- pool[i] - 1
        Array.append pool (Array.create newFish 8)
    [|1..80|]
    |> Array.fold folder processed
    |> Array.length
*)