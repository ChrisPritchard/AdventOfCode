module Day05

open Common
open System

let processed = readEmbedded "day05" |> Array.map (fun s ->
    let parts = split ", ->" s |> Array.map int
    parts[0],parts[1],parts[2],parts[3])

let init () =
    processed |> Array.length |> ignore

let asPoints (x1,y1,x2,y2) =
    let dx = if x1 < x2 then 1 else -1
    let dy = if y1 < y2 then 1 else -1
    if x1 = x2 then
        [|y1..dy..y2|] |> Array.map (fun y -> x1, y)
    else if y1 = y2 then
        [|x1..dx..x2|] |> Array.map (fun x -> x, y1)
    else
        Array.zip [|x1..dx..x2|] [|y1..dy..y2|]

let part1 () =
    processed 
    |> Array.filter (fun (x1,y1,x2,y2) -> (x1 = x2 && y1 <> y2) || (x1 <> x2 && y1 = y2))
    |> Array.collect asPoints
    |> Array.groupBy id
    |> Array.filter (fun (_, a) -> Array.length a > 1)
    |> Array.length

let part2 () =
    processed 
    |> Array.collect asPoints
    |> Array.groupBy id
    |> Array.filter (fun (_, a) -> Array.length a > 1)
    |> Array.length
