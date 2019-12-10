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
let blockers = Set.ofArray asteroids

    
let part1 () =

    let step = 0.5

    let lineOfSight (x, y) (tx, ty) =

        if (x, y) = (tx, ty) then false
        else
            let angle = (float ty - float y) / step |> sin
            let nx = step * cos angle
            let ny = step * sin angle

            let rec crawl (ox, oy) lastDist =
                let rounded = (int ox, int oy)
                if rounded = (tx, ty) then true
                elif rounded <> (x, y) && Set.contains (int ox, int oy) blockers then false
                else 
                    let dist = sqrt ((float tx - ox) ** 2. + (float ty - oy) ** 2.)
                    if dist > lastDist then 
                        false
                    else
                        crawl (ox + nx, oy + ny) dist

            crawl (float x, float y) System.Double.MaxValue

    let maxReachable (x, y) =
        asteroids |> Seq.filter (lineOfSight (x, y)) |> Seq.length
    
    asteroids |> Seq.map maxReachable |> Seq.max

let part2 () =
    
    0