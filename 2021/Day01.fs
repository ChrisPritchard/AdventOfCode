module Day01

open Common

let processed = readEmbedded "day01" |> Array.map int

let init () =
    processed |> Array.length |> ignore

let part1 () =
    let mutable count = 0
    let mutable last = processed[0]
    for i = 1 to processed.Length - 1 do
        if processed[i] > last then
            count <- count + 1
        last <- processed[i]
    count

let part2 () =
    let mutable count = 0
    let mutable last3 = processed[0]
    let mutable last2 = processed[1]
    let mutable last1 = processed[2]
    for i = 3 to processed.Length - 1 do
        if processed[i] > last3 then
            count <- count + 1
        last3 <- last2
        last2 <- last1
        last1 <- processed[i]
    count

// initial version

// let part1 () =
//     processed
//     |> Array.windowed 2
//     |> Array.filter (fun set -> set[1] > set[0])
//     |> Array.length

// let part2 () =
//     processed
//     |> Array.windowed 3
//     |> Array.map Array.sum
//     |> Array.windowed 2
//     |> Array.filter (fun set -> set[1] > set[0])
//     |> Array.length
    
// folder version, one iteration (actually slower lol):

// let part1 () =
//     let folder (last, total) next = 
//         if next > last then (next, total + 1) else (next, total)
//     processed
//     |> Array.fold folder (9999, 0) |> snd

// let part2 () =
//     let folder (last3, last2, last1, total) next =
//         if next > last3 then (last2, last1, next, total + 1) else (last2, last1, next, total)
//     processed
//     |> Array.fold folder (9999, 9999, 9999, 0) |> fun (_,_,_,total) -> total
