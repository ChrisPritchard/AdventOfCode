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
    (x2-x1) * (y2-y1) * (z2-z1)

let part1 () =
    let cap (x1,x2,y1,y2,z1,z2) =
        max -50L x1, min 50L x2, max -50L y1, min 50L y2, max -50L z1, min 50L z2

    let rec cull (toSplit: Coord) (against: Coord) keepInner  =
        let (x1,x2,y1,y2,z1,z2) = toSplit
        let (xx1,xx2,yy1,yy2,zz1,zz2) = against
        if x1 > xx2 || y1 > yy2 || z1 > zz2 || x2 < xx1 || y2 < yy1 || z2 < zz1 then
            if keepInner then
                [|toSplit; against|] // no overlap
            else
                [|toSplit|]
        else if x1 < xx1 then
            Array.append [|x1,xx1,y1,y2,z1,z2|] (cull (xx1,x2,y1,y2,z1,z2) against keepInner)
        else if x2 > xx2 then
            Array.append [|xx2,x2,y1,y2,z1,z2|] (cull (x1,xx2,y1,y2,z1,z2) against keepInner)
        else if y1 < yy1 then
            Array.append [|x1,x2,y1,yy1,z1,z2|] (cull (x1,x2,yy1,y2,z1,z2) against keepInner)
        else if y2 > yy2 then
            Array.append [|x2,x2,yy2,y2,z1,z2|] (cull (x1,x2,y1,yy2,z1,z2) against keepInner) 
        else if z1 < zz1 then
            Array.append [|x1,x2,y1,y2,z1,zz1|] (cull (x1,x2,y1,y2,zz1,z2) against keepInner) 
        else if z2 > zz2 then
            Array.append [|x1,x2,y1,y2,zz2,z2|] (cull (x1,x2,y1,y2,z1,zz2) against keepInner) 
        else if keepInner then
            [|against|] // assume toSplit is inside against
        else
            Array.empty

    let rec remove (toRemove: Coord) (from: Coord)  =
        let (x1,x2,y1,y2,z1,z2) = toRemove
        let (xx1,xx2,yy1,yy2,zz1,zz2) = from
        if x1 > xx2 || y1 > yy2 || z1 > zz2 || x2 < xx1 || y2 < yy1 || z2 < zz1 then
            [|from|] // no overlap
        else if x1 < xx1 then
            remove (xx1,x2,y1,y2,z1,z2) from
        else if x2 > xx2 then
            remove (x1,xx2,y1,y2,z1,z2) from
        else if y1 < yy1 then
            remove (x1,x2,yy1,y2,z1,z2) from
        else if y2 > yy2 then
            remove (x1,x2,y1,yy2,z1,z2) from
        else if z1 < zz1 then
            remove (x1,x2,y1,y2,zz1,z2) from
        else if z2 > zz2 then
            remove (x1,x2,y1,y2,z1,zz2) from
        else
            // if toRemove is inside... do the split op but discard the inner
            cull from toRemove false

    let rec noOverlaps shape (others: Coord[]) =
        let mutable j = 0
        let mutable lastCull = Array.empty
        while j < others.Length && lastCull.Length <= 2 do
            lastCull <- cull shape others[j] true
            j <- j + 1
        if lastCull.Length = 1 then
            others
        else if lastCull.Length = 2 then
            Array.append others [|lastCull[1]|]
        else
            (lastCull[0..lastCull.Length - 2], others)
            ||> Array.fold (fun others shape -> noOverlaps shape others)

    let all = 
        coords
        |> Array.fold (fun (acc: Coord[]) (on, coord: Coord) ->
            if on then 
                if Array.isEmpty acc then
                    [|coord|]
                else
                    noOverlaps coord acc
            else
                Array.collect (fun o -> remove coord o) acc) Array.empty
    
    all |> Array.sumBy size

let part2 () =
    0