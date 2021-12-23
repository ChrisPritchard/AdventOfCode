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
    |> fun a -> a[0] = 1, (a[1],a[2],a[3],a[4],a[5],a[6]))

let size (x1,x2,y1,y2,z1,z2) =
    (x2-x1) * (y2-y1) * (z2-z1)

let intersect (x1,x2,y1,y2,z1,z2) (xx1,xx2,yy1,yy2,zz1,zz2) =
    if x1 > xx2 || y1 > yy2 || z1 > zz2 || x2 < xx1 || y2 < yy1 || z2 < zz1 then
        0, 0, 0, 0, 0, 0 // no overlap
    else
        max x1 xx1, min x2 xx2, max y1 yy1, min y2 yy2, max z1 zz1, min z2 zz2

let noOverlaps a b =
    let overlap = intersect a b
    if size overlap = 0 then
        [|a;b|] // no overlap
    else if size a < size b then
        // split up a and add to b
        let (x1,x2,y1,y2,z1,z2) = a
        let (xx1,xx2,yy1,yy2,zz1,zz2) = intersect a overlap
        [|
            b
        |]
    else
        // split up b and add to a
        [|
            a
        |]

let part1 () =
    let cap (x1,x2,y1,y2,z1,z2) =
        max -50 x1, min 50 x2, max -50 y1, min 50 y2, max -50 z1, min 50 z2

    let within (x,y,z) (x1,x2,y1,y2,z1,z2) =
        x >= x1 && x <= x2 && y >= y1 && y <= y2 && z >= z1 && z <= z2
    
    let mutable count = 0
    for x = -50 to 50 do
        for y = -50 to 50 do
            for z = -50 to 50 do
                let mutable i = coords.Length - 1
                while i > -1 do
                    let on, cube = coords[i]
                    let valid = within (x, y, z) cube
                    if on && valid then
                        count <- count + 1
                        i <- -1
                    else if valid then
                        i <- -1
                    else
                        i <- i - 1
    
    count

let part2 () =
    0