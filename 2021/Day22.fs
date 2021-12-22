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

let intersect (x1,x2,y1,y2,z1,z2) (xx1,xx2,yy1,yy2,zz1,zz2) =
    if x1 > xx2 || y1 > yy2 || z1 > zz2 || x2 < xx1 || y2 < yy1 || z2 < zz1 then
        0, 0, 0, 0, 0, 0 // no overlap
    else
        max x1 xx1, min x2 xx2, max y1 yy1, min y2 yy2, max z1 zz1, min z2 zz2

let part1 () =
    let cap (x1,x2,y1,y2,z1,z2) =
        max -50 x1, min 50 x2, max -50 y1, min 50 y2, max -50 z1, min 50 z2
    
    let mutable count = 0
    for i = [0..coords.Length - 1] do
        let (on, coord) = coords[i]
        let coord = cap coord
        let sized = size coord
        if on then
            // go through previous 'on's and find intersections
            // only add for size - intersection
            // if there is an 

let part2 () =
    0