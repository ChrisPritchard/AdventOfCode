module Day22

open Common
open System

let processed = readEmbedded "day22"

let init () =
    processed |> Array.length |> ignore

type Coord = int64*int64*int64*int64*int64*int64

let coords = processed |> Array.map (fun s -> 
    s.Replace("on", "1").Replace("off","0") 
    |> split " x=.,yz" 
    |> Array.map int64 
    |> fun a -> a[0] = 1, (a[1],a[2],a[3],a[4],a[5],a[6]))

let size ((x1,x2,y1,y2,z1,z2): Coord) =
    (x2-x1+1L) * (y2-y1+1L) * (z2-z1+1L)

let rec cull shapes target =
    let (tx1,tx2,ty1,ty2,tz1,tz2) = target
    shapes
    |> Array.collect (fun shape ->
        let (x1,x2,y1,y2,z1,z2) = shape
        if x2 < tx1 || y2 < ty1 || z2 < tz1 || x1 > tx2 || y1 > ty2 || z1 > tz2 then
            [|shape|]   // completely outside
        else if x1 >= tx1 && x2 <= tx2 && y1 >= ty1 && y2 <= ty2 && z1 >= tz1 && z2 <= tz2 then
            Array.empty // completely inside
        else if x1 < tx1 then
            Array.append [|x1, tx1-1L, y1, y2, z1, z2|] (cull [|tx1, x2, y1, y2, z1, z2|] target)
        else if x2 > tx2 then
            Array.append [|tx2+1L, x2, y1, y2, z1, z2|] (cull [|x1, tx2, y1, y2, z1, z2|] target)
        else if y1 < ty1 then
            Array.append [|x1, x2, y1, ty1-1L, z1, z2|] (cull [|x1, x2, ty1, y2, z1, z2|] target)
        else if y2 > ty2 then
            Array.append [|x1, x2, ty2+1L, y2, z1, z2|] (cull [|x1, x2, y1, ty2, z1, z2|] target)
        else if z1 < tz1 then
            Array.append [|x1, x2, y1, y2, z1, tz1-1L|] (cull [|x1, x2, y1, y2, tz1, z2|] target)
        else if z2 > tz2 then
            Array.append [|x1, x2, y1, y2, tz2+1L, z2|] (cull [|x1, x2, y1, y2, z1, tz2|] target)
        else
            failwith "should not be reached")

let instructionFolder acc (on, coord) =
    if on && Array.isEmpty acc then [|coord|]
    else
        if on then 
            let split = Array.fold cull [|coord|] acc
            Array.append acc split
        else
            Array.fold cull acc [|coord|]

let part1 () =

    coords
    |> Array.filter (fun (_, (x1,x2,y1,y2,z1,z2)) -> 
        not (x1 > 50L || x2 < -50L || y1 > 50L || y2 < -50L || z1 > 50L || z2 < -50L))
    |> Array.map (fun (on, (x1,x2,y1,y2,z1,z2)) ->
        on, (max x1 -50L, min x2 50L, max y1 -50L, min y2 50L, max z1 -50L, min z2 50L))
    |> Array.fold instructionFolder Array.empty
    |> Array.sumBy size

let part2 () =
    
    coords
    |> Array.fold instructionFolder Array.empty
    |> Array.sumBy size