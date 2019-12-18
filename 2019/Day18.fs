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

let part1 () =

    let edges visited (x, y) =
        [ 0, -1; 0, 1; 1, 0; -1, 0 ]
        |> Seq.map (fun (dx, dy) -> x + dx, y + dy)
        |> Seq.filter (fun (ox, oy) -> ox >= 0 && oy >= 0 && ox < map.[0].Length && oy < map.Length)
        |> Seq.filter (fun (ox, oy) ->
            let tile = map.[oy].[ox]
            tile <> '#' && (not (Char.IsLetter tile) || Char.IsLower tile || Set.contains (Char.ToLower tile) visited))

    let isGoal visited (x, y) =
        let tile = map.[y].[x]
        Char.IsLetter tile
        && Char.IsLower tile
        && not (Set.contains tile visited)

    let rec runner cnt visited start =
        let search = BFS.run (isGoal visited) (edges visited) start
        match search with
        | None -> cnt
        | Some path ->
            let (fx, fy) = List.last path
            let tile = map.[fy].[fx]
            runner (cnt + path.Length - 1) (Set.add tile visited) (fx, fy)

    runner 0 Set.empty start

let part2 () =

    0