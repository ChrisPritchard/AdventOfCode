module Day18

open Common
open System
open System.IO
open System.Collections.Generic

let input = File.ReadAllLines ("./inputs/day18.txt")

(*
I needed to get some ideas for this one. Since you need to find the optimum path, this is basically the travelling salesman problem. 
A bfs/dfs based solution would run forever on non-trivial inputs, like one of the samples and of course the input data.

Looking around on reddit, I was able to figure out two key optimisations:

- First, the path between two keys can be treated as a single static value: it doesnt change, as long as you track the keys for doors in between.
    This allows all these paths to be precomputed which speeds things up.
- Second, for any given position and collected key set, track the optimal step count to reach that point. 
    If when processing you find a state has been reached before with better time, you can stop immediately.

The latter was probably the critical insight. That plus ditching recursion for mutable collections resulted in a ~6 second runtime.
This could (definitely) be improved with micro optimisations, but I'm happy.
*)

let paths (start: char, startPos: int * int) others (map: char [][]) =
    let edges (x, y) =
        [ 0, -1; 0, 1; 1, 0; -1, 0 ]
        |> Seq.map (fun (dx, dy) -> x + dx, y + dy)
        |> Seq.filter (fun (ox, oy) -> ox >= 0 && oy >= 0 && ox < map.[0].Length && oy < map.Length)
        |> Seq.filter (fun (ox, oy) ->
            let tile = map.[oy].[ox]
            tile <> '#')
    let canReach = 
        others 
        |> Array.choose (fun (goal, goalPos) ->
            if goal = start then None
            else 
                BFS.run ((=) goalPos) edges startPos
                |> Option.map (fun path -> path, goal))
    canReach
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

let part1 () =
    
    let map = input |> Array.map (fun s -> s.ToCharArray ())
    let keys = 
        map 
        |> Array.indexed 
        |> Array.collect (fun (y, line) -> 
            line 
            |> Array.indexed 
            |> Array.filter (snd >> Char.IsLower) 
            |> Array.map (fun (x, c) -> c, (x, y)))

    let start = 
        [0..map.Length - 1] 
        |> List.pick (fun y -> map.[y] |> Array.tryFindIndex (fun c -> c = '@') |> Option.map (fun x -> x, y))
    let startOptions = paths ('@', start) keys map
    let keyPairs = keys |> Array.map (fun (key, keyPos) -> key, paths (key, keyPos) keys map) |> Map.ofArray
    let map = Map.add '@' startOptions keyPairs

    let finalStates = Dictionary<(Set<char> * char), int>()
    let target = keys |> Array.map fst |> Set.ofArray
    let queue = Queue<Set<char> * int * char>([Set.empty, 0, '@'])
    let mutable globalMin = Int32.MaxValue

    let processNext visited acc current = 
        if visited = target then
            if acc < globalMin then globalMin <- acc
        else
            finalStates.[(visited, current)] <- acc
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

    globalMin

(*
Part 2 required no hints, as with a bit of hackery its just part 1 again with a slightly different state variable:
Rather than tracking the current key, I track a tuple of the current four keys. Again it finishes in roughly 5-6 seconds.

I did make a mistake where I misrote the replacement code, mangling the map and causing an essentually infinite brute force again, but that was solved quickly :)
*)

let part2 () =

    let map = input |> Array.map (fun s -> s.ToCharArray ())
    let (sx, sy) = 
        [0..map.Length - 1] 
        |> List.pick (fun y -> map.[y] |> Array.tryFindIndex (fun c -> c = '@') |> Option.map (fun x -> x, y))
    
    let replace = """1#2
                     ###
                     3#4""" |> split "\r\n\t " |> Array.map (fun s -> s.ToCharArray())
    for y = -1 to 1 do
        for x = -1 to 1 do
            map.[sy + y].[sx + x] <- replace.[y + 1].[x + 1]

    //for y = 0 to map.Length - 1 do
    //    for x = 0 to map.[y].Length - 1 do
    //        printf "%c" map.[y].[x]
    //    printfn ""

    let keys = 
        map 
        |> Array.indexed 
        |> Array.collect (fun (y, line) -> 
            line 
            |> Array.indexed 
            |> Array.filter (snd >> Char.IsLower) 
            |> Array.map (fun (x, c) -> c, (x, y))) 

    let startOptions1 = paths ('1', (sx - 1, sy - 1)) keys map
    let startOptions2 = paths ('2', (sx + 1, sy - 1)) keys map
    let startOptions3 = paths ('3', (sx - 1, sy + 1)) keys map
    let startOptions4 = paths ('4', (sx + 1, sy + 1)) keys map
    let keyPairs = keys |> Array.map (fun (key, keyPos) -> key, paths (key, keyPos) keys map) |> Map.ofArray
    let map = 
        keyPairs
        |> Map.add '1' startOptions1
        |> Map.add '2' startOptions2
        |> Map.add '3' startOptions3
        |> Map.add '4' startOptions4

    let finalStates = Dictionary<(Set<char> * char * char * char * char), int>()
    let target = keys |> Array.map fst |> Set.ofArray
    let queue = Queue<Set<char> * int * char * char * char * char>([Set.empty, 0, '1', '2', '3', '4'])
    let mutable globalMin = Int32.MaxValue

    let processNext visited acc (a, b, c, d) = 
        if visited = target then
            if acc < globalMin then globalMin <- acc
        else
            finalStates.[(visited, a, b, c, d)] <- acc
            Array.concat [|
                map.[a] |> Array.map (fun o -> a, o);
                map.[b] |> Array.map (fun o -> b, o);
                map.[c] |> Array.map (fun o -> c, o);
                map.[d] |> Array.map (fun o -> d, o)
                |]
            |> Array.filter (fun (_, (key, (_, (_, requiredKeys)))) -> 
                not (Set.contains key visited) && Set.isEmpty (Set.difference requiredKeys visited))
            |> Array.iter (fun (source, (key, (cnt, (newKeys, _)))) -> 
                let newVisited = (visited, newKeys) ||> Set.foldBack Set.add
                let a = if source = a then key else a
                let b = if source = b then key else b
                let c = if source = c then key else c
                let d = if source = d then key else d
                let toEnqueue = newVisited, (acc + cnt), a, b, c, d
                queue.Enqueue toEnqueue)

    while queue.Count > 0 do
        let (visited, acc, a, b, c, d) = queue.Dequeue ()
        if not (finalStates.ContainsKey (visited, a, b, c, d)) || finalStates.[(visited, a, b, c, d)] > acc then
            processNext visited acc (a, b, c, d)

    globalMin