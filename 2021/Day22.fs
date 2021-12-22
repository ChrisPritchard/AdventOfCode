module Day22

open Common
open System

let processed = readEmbedded "day22"

let init () =
    processed |> Array.length |> ignore

let coords = processed |> Array.map (fun s -> 
    s.Replace("on", "1").Replace("off","0") 
    |> split " x=.,yz" 
    |> Array.map int 
    |> fun a -> a[0] = 1, (a[1],a[2],a[3],a[4],a[4],a[5]))

let size (x1,x2,y1,y2,z1,z2) =
    (x2-x1) * (y2-y1) * (z2-z1)

let part1 () =
    let cap (x1,x2,y1,y2,z1,z2) =
        max -50 x1, min 50 x2, max -50 y1, min 50 y2, max -50 z1, min 50 z2
    coords

let part2 () =
    0