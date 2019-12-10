module Day10

open System.IO

let input = File.ReadAllLines ("./inputs/day10.txt")
let asteroids = 
    input 
    |> Seq.mapi (fun y line ->
        line |> Seq.mapi (fun x cell -> (x, y), cell = '#') |> Seq.filter snd |> Seq.map fst)
    |> Seq.collect id
    |> Seq.toArray
    
let part1 () =

    let angle (x1, y1) (x2, y2) =
        ((float y2 - float y1) / (float x2 - float x1)) |> tanh

    let dist (x1, y1) (x2, y2) =
        (pown (x2 - x1) 2 + pown (y2 - y1) 2) |> float |> sqrt
        
    let lineOfSight a b =
        if a = b then false
        else
            let angleAB = angle a b
            let distAB = dist a b
            asteroids 
            |> Seq.exists (fun c -> c <> a && c <> b && angle a c = angleAB && dist a c < distAB)
            |> not

    let maxReachable (x, y) =
        asteroids |> Seq.filter (lineOfSight (x, y)) |> Seq.length
    
    asteroids |> Seq.map maxReachable |> Seq.max

let part2 () =
    
    0