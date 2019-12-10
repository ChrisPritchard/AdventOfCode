module Day10

open Common
open System.IO
open System.Collections.Generic

type T = int64
type TQueue = Queue<T>
let stot (o: string) = int64 o
let itot (o: int) = int64 o

let input = File.ReadAllLines ("./inputs/day10.txt")
let asteroids = 
    input 
    |> Seq.mapi (fun y line ->
        line |> Seq.mapi (fun x cell -> (x, y), cell = '#') |> Seq.filter snd |> Seq.map fst)
    |> Seq.collect id
    |> Seq.toArray
    
let part1 () =

    let cache = Dictionary<((int * int) * (int * int)), float>()
    let dist (x1, y1) (x2, y2) =
        let key = (x1, y1), (x2, y2)
        let keyi = (x2, y2), (x1, y1)
        if cache.ContainsKey key then cache.[key]
        elif cache.ContainsKey keyi then cache.[keyi]
        else
            let d = sqrt ((float x2 - float x1) ** 2. + (float y2 - float y1) ** 2.)
            cache.Add (key, d)
            d

    let margin = 0.0001

    let lineOfSight a b =
        a <> b &&
        asteroids 
        |> Seq.exists (fun c -> c <> a && c <> b && abs ((dist a c + dist b c) - dist a b) <= margin)
        |> not

    let maxReachable (x, y) =
        asteroids |> Seq.filter (lineOfSight (x, y)) |> Seq.length
    
    asteroids |> Seq.map maxReachable |> Seq.max

let part2 () =
    
    0