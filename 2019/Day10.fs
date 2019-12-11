module Day10

open System
open System.IO

let input = File.ReadAllLines ("./inputs/day10.txt")

let asteroids = 
    input 
    |> Seq.mapi (fun y line ->
        line |> Seq.mapi (fun x cell -> (x, y), cell = '#') |> Seq.filter snd |> Seq.map fst)
    |> Seq.collect id
    |> Seq.toArray

let simplify (dx, dy) =
    if dx = 0 then 0, dy / abs dy
    elif dy = 0 then dx / abs dx, 0
    else
        let maxDivisor = [abs dy..(-1)..1] |> List.find (fun i -> dx % i = 0 && dy % i = 0)
        dx / maxDivisor, dy / maxDivisor

let linesOfSight (ox, oy) (visible, blocked) (x, y) =
    if Set.contains (x, y) blocked || (ox, oy) = (x, y) then (visible, blocked)
    else
        let dx, dy = simplify (x - ox, y - oy)
        let rec ray (visible, blocked) (nx, ny) =
            if nx < 0 || ny < 0 || nx >= input.[0].Length || ny >= input.Length then
                visible, blocked
            else
                ray (Set.remove (nx, ny) visible, Set.add (nx, ny) blocked) (nx + dx, ny + dy)
        ray (Set.add (x, y) visible, blocked) (x + dx, y + dy)

let visible map (x, y) =
    ((Set.empty, Set.empty), map) ||> Array.fold (linesOfSight (x, y))
    |> fst
    
let (laserBase, part1Answer) = 
    asteroids |> Seq.map (fun o -> o, visible asteroids o |> Set.count) |> Seq.maxBy snd

// note: a simpler way to calculate visible is grouping by angle, as below

(*
let angle (x1, y1) (x2, y2) = 
    let a = (atan2 (float (y2 - y1)) (float (x2 - x1))) * 180./Math.PI
    if a < 0. then 360. + a else a

let visible map (x, y) =
    let distance (ox, oy) = ((pown (ox - x) 2) + (pown (oy - y) 2)) |> float |> sqrt
    map |> Array.except [|x, y|] |> Array.groupBy (angle (x, y)) |> Array.map (snd >> Array.minBy distance)

let laserBase, part1Answer = asteroids |> Seq.map (fun o -> o, visible asteroids o |> Array.length) |> Seq.maxBy snd
*)

let part1 () =
    
    part1Answer

let part2 () =
    
    let angle (x1, y1) (x2, y2) = 
        let a = (atan2 (float (y2 - y1)) (float (x2 - x1))) * 180./Math.PI
        if a < 0. then 360. + a else a

    let angleNorth o1 o2 =
        let b = angle o1 o2 + 90.
        if b >= 360. then b - 360. else b

    let targetCount = 200

    let rec laser targets map count = 
        match targets with
        | [] ->
            let newVisible = 
                visible (Seq.toArray map) laserBase 
                |> Seq.sortBy (angleNorth laserBase) 
                |> Seq.toList
            laser newVisible map count
        | next::_ when count = targetCount - 1 ->
            next
        | next::remaining ->
            laser remaining (Set.remove next map) (count + 1)

    let x, y = laser [] (Set.ofArray asteroids) 0
    x * 100 + y

