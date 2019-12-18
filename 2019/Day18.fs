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
let keys = map |> Array.collect (fun line -> line |> Array.filter (fun c -> Char.IsLetter c && Char.IsLower c))

let part1 () =

    let edges visited (x, y) =
        [ 0, -1; 0, 1; 1, 0; -1, 0 ]
        |> Seq.map (fun (dx, dy) -> x + dx, y + dy)
        |> Seq.filter (fun (ox, oy) -> ox >= 0 && oy >= 0 && ox < map.[0].Length && oy < map.Length)
        |> Seq.filter (fun (ox, oy) ->
            let tile = map.[oy].[ox]
            tile <> '#' && (not (Char.IsLetter tile) || Char.IsLower tile || Set.contains (Char.ToLower tile) visited))

    let rec runner cnt visited start targets =
        targets 
        |> Array.filter (fun tile -> not (Set.contains tile visited))
        |> Array.choose (fun tile -> 
            BFS.run (fun (x, y) -> map.[y].[x] = tile) (edges visited) start)
        |> Array.map (fun path ->
            let (fx, fy) = List.last path
            let tile = map.[fy].[fx]
            cnt + path.Length - 1, Set.add tile visited, (fx, fy))
        |> Array.map (fun (cnt, visited, start) ->
            if Set.count visited = targets.Length then cnt
            else
                runner cnt visited start targets)
        |> Array.min

    runner 0 Set.empty start keys

let part2 () =

    0