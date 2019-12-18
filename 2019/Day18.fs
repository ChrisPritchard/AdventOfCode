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
                |> Array.map Char.ToLower
            //let others = path |> List.map (fun (x, y) -> map.[y].[x]) |> List.filter Char.Lower
            goal, (path.Length - 1, Set.ofArray blocks))
        |> Array.sortBy (snd >> fst)
        
    let startOptions = paths ('@', start) keys
    let keyPairs = keys |> Array.map (fun (key, keyPos) -> key, paths (key, keyPos) keys) |> Map.ofArray
    let map = Map.add '@' startOptions keyPairs

    let finalStates = Dictionary<(Set<char> * char), int>()
    let target = keys |> Array.map fst |> Set.ofArray
    let queue = Queue<Set<char> * int * char>([Set.empty, 0, '@'])

    while queue.Count > 0 do
        let (visited, acc, current) = queue.Dequeue ()
        if not (finalStates.ContainsKey (visited, current)) || finalStates.[(visited, current)] > acc then
            finalStates.[(visited, current)] <- acc
            if visited.Count < keys.Length then
                map.[current] 
                |> Array.filter (fun (key, (_, blocks)) -> 
                    not (Set.contains key visited) && Set.isEmpty (Set.difference blocks visited))
                |> Array.iter (fun (key, (cnt, _)) -> 
                    let toEnqueue = (Set.add key visited), (acc + cnt), key
                    queue.Enqueue toEnqueue)

    finalStates |> Seq.choose (fun o -> if fst o.Key = target then Some o.Value else None) |> Seq.min

let part2 () =

    0