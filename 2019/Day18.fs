module Day18

open Common
open System
open System.IO
open System.Collections.Generic

let input = File.ReadAllLines ("./inputs/day18.txt")

let map = input |> Array.map (fun s -> s.ToCharArray ())
let start = 
    [0..map.Length - 1] 
    |> List.pick (fun y -> map.[y] |> Array.tryFindIndex (fun c -> c = '@') |> Option.map (fun x -> x, y))
let keys = 
    map 
    |> Array.indexed 
    |> Array.collect (fun (y, line) -> 
        line 
        |> Array.indexed 
        |> Array.filter (snd >> Char.IsLower) 
        |> Array.map (fun (x, c) -> c, (x, y)))

let part1 () =

    // new approach:
    // for each key, calculate distance to all other keys and start
        // when calculating distance, also add up blocked doors
        // also incidental keys collected?
    // with this cache, then do a search to find shortest path?

    let paths (start: char, startPos: int * int) others =
        let edges (x, y) =
            [ 0, -1; 0, 1; 1, 0; -1, 0 ]
            |> Seq.map (fun (dx, dy) -> x + dx, y + dy)
            |> Seq.filter (fun (ox, oy) -> ox >= 0 && oy >= 0 && ox < map.[0].Length && oy < map.Length)
            |> Seq.filter (fun (ox, oy) ->
                let tile = map.[oy].[ox]
                tile <> '#')
        others 
        |> Array.choose (fun (goal, goalPos) ->
            if goal = start then None
            else 
                BFS.run ((=) goalPos) edges startPos 
                |> Option.map (fun path -> goal, path))
        |> Array.map (fun (goal, path) ->
            let blocks = 
                path 
                |> List.map (fun (x, y) -> map.[y].[x]) 
                |> List.filter Char.IsUpper |> List.toArray
            //let others = path |> List.map (fun (x, y) -> map.[y].[x]) |> List.filter Char.Lower
            (start, goal), (path.Length - 1, blocks))
        
    let startOptions = paths ('@', start) keys
    let keyPairs = keys |> Array.collect (fun key -> paths key keys)

    0

let part2 () =

    0