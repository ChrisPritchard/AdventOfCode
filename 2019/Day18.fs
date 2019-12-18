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
                |> Option.map (fun path -> path, goal))
        |> Array.map (fun (path, goal) ->
            let (newKeys, requiredKeys) = 
                ((Set.empty, Set.empty), path)
                ||> List.fold (fun (newKeys, requiredKeys) (x, y) ->
                    let tile = map.[y].[x]
                    if Char.IsLower tile then
                        Set.add tile newKeys, requiredKeys
                    elif Char.IsUpper tile && not (Set.contains (Char.ToLower tile) newKeys) then
                        newKeys, Set.add (Char.ToLower tile) requiredKeys
                    else
                        newKeys, requiredKeys)
            goal, (path.Length - 1, (newKeys, requiredKeys)))
        |> Array.sortBy (snd >> fst)
        
    let startOptions = paths ('@', start) keys
    let keyPairs = keys |> Array.map (fun (key, keyPos) -> key, paths (key, keyPos) keys) |> Map.ofArray
    let map = Map.add '@' startOptions keyPairs

    let finalStates = Dictionary<(Set<char> * char), int>()
    let target = keys |> Array.map fst |> Set.ofArray
    let queue = Queue<Set<char> * int * char>([Set.empty, 0, '@'])

    let processNext visited acc current = 
        finalStates.[(visited, current)] <- acc
        if visited.Count < keys.Length then
            map.[current] 
            |> Array.filter (fun (key, (_, (_, requiredKeys))) -> 
                not (Set.contains key visited) && Set.isEmpty (Set.difference requiredKeys visited))
            |> Array.iter (fun (key, (cnt, (newKeys, _))) -> 
                let newVisited = (visited, newKeys) ||> Set.foldBack Set.add
                let toEnqueue = newVisited, (acc + cnt), key
                queue.Enqueue toEnqueue)

    while queue.Count > 0 do
        let (visited, acc, current) = queue.Dequeue ()
        if not (finalStates.ContainsKey (visited, current)) || finalStates.[(visited, current)] > acc then
            processNext visited acc current

    finalStates |> Seq.choose (fun o -> if fst o.Key = target then Some o.Value else None) |> Seq.min

let part2 () =

    0