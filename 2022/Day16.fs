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

    let possiblePaths pos left timeLeft = 
        left 
        |> Seq.filter (fun p -> distToValve pos p < timeLeft)

    let rec allPaths acc = 

        // time to brain
        // // calculate all paths
        // any path that runs out of other valves or has no possible additional paths, can have its score calculated
        // then there are some remainder paths

        let next = 
            acc |> List.collect (fun (path, time) -> 
                match path with
                | [] -> failwith "not possible"
                | last::_ -> 
                    let newPaths = 
                        possiblePaths last (List.except path valves) time
                        |> Seq.map (fun p -> (p::path), time - distToValve last p)
                        |> Seq.toList
                    if List.isEmpty newPaths then [path, time]
                    else newPaths)

        // add a fold. given the current paths, fold to find the next paths and current max
        // if no next paths, return current max, else, recurse with new paths and max

        next

    let rec findBest pos left timeLeft score =
        let nextBest, dist, scoreToAdd = 
            left 
            |> Seq.map (fun p -> 
                let dist = distToValve pos p
                p, dist, (timeLeft - dist) * valveValues.[p])
            |> Seq.maxBy (fun (_, _, v) -> v)
        printfn "from %s to %s in %d minutes" pos nextBest dist
        let left = List.except [nextBest] left
        if List.isEmpty left then score + scoreToAdd
        else
            findBest nextBest left (timeLeft - dist) (score + scoreToAdd)

    findBest "AA" valves 30 0

    // while there are too many possible paths to test them all, there are less valves with actual pressure values
    // you would never visit a pressure valve more than once. 720 in the source data, 2004310016 in the real data 
    //  (which might be barely attainable, especially with eager discarding of ineffient paths)
        
    // from a given pos, with a given score and time, return the best of all available options...
    // from AA there are a bunch of possibles, with dists and scores for each
    // going to be all valves except those opened, with a cost

let part2 () =
    0