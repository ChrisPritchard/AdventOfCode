module Day16

open Common
open System.Collections.Generic

let bfs<'T when 'T : equality> (isGoal: 'T -> bool) (edges: 'T -> seq<'T>) (start: 'T) =
    let queue = Queue<'T>()
    let discovered = HashSet<'T>()
    let parents = Dictionary<'T, 'T>()
    
    discovered.Add start |> ignore
    queue.Enqueue start

    let rec reconstructPath acc v =
        if parents.ContainsKey v then
            reconstructPath (v::acc) parents.[v]
        else
            v::acc

    let rec searcher () =
        if queue.Count = 0 then None
        else
            let v = queue.Dequeue ()
            if isGoal v then
                reconstructPath [] v |> Some
            else
                let edges = edges v
                for w in edges do
                    if not (discovered.Contains w) then
                        discovered.Add w |> ignore
                        parents.Add (w, v)
                        queue.Enqueue w
                searcher ()
            
    searcher ()

let part1 () =
    let input = 
        readEmbeddedRaw "day16"
        |> Seq.map (fun line ->
            line
                .Replace("Valve ", "")
                .Replace(" has flow rate=", ",")
                .Replace(" has flow rate=", ",")
                .Replace("; tunnels lead to valves ", ",")
                .Replace("; tunnel leads to valve ", ",")
                .Replace(", ", ",")
            |> split ",")
    
    let valveValues = 
        input 
        |> Seq.choose (fun a -> 
            if a[1] = "0" then None 
            else Some (a[0], int a[1]))
        |> Map.ofSeq
    let valves = Map.keys valveValues |> List.ofSeq

    let paths =
        input 
        |> Seq.map (fun a -> a[0], a[2..])
        |> Map.ofSeq

    let mutable memoDists = Map.empty
    let distToValve start target = 
        if Map.containsKey (start, target) memoDists then
            memoDists.[start, target]
        else
            let isGoal = (=) target
            let edges p = Map.find p paths |> Array.toSeq
            let path = bfs isGoal edges start
            let dist = List.length path.Value
            memoDists <- Map.add (start, target) dist memoDists
            dist

    let scoreForPath path =
        ((0, 30), List.windowed 2 path)
        ||> List.fold (fun (score, timeLeft) pair ->
            let dist = distToValve pair[0] pair[1]
            let timeLeft = timeLeft - dist
            score + timeLeft * valveValues[pair[1]], timeLeft)
        |> fst

    let timeLeft path =
        (30, List.windowed 2 path)
        ||> List.fold (fun timeLeft pair -> timeLeft - distToValve pair[0] pair[1])

    let possiblePaths path = 
        let timeLeft = timeLeft path
        valves 
        |> List.except path 
        |> List.filter (fun p -> distToValve (List.head path) p < timeLeft) 
        |> List.map (fun p -> p::path)

    let rec highScore paths =
        paths 
        |> Seq.map (fun p ->
            let next = possiblePaths p
            if List.isEmpty next then List.rev p |> scoreForPath
            else
                highScore next)
        |> Seq.max

    highScore [["AA"]]

let part2 () =
    0