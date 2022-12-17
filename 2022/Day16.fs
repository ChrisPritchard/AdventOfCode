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
            |> split ",")
    
    let valves = 
        input 
        |> Seq.choose (fun a -> 
            if a[1] = "0" then None 
            else Some (a[0], int a[1]))
        |> Array.ofSeq

    let paths =
        input 
        |> Seq.map (fun a -> a[0], a[2..])
        |> Map.ofSeq

    let start = "AA"

    // while there are too many possible paths to test them all, there are less valves with actual pressure values
    // you would never visit a pressure valve more than once. 720 in the source data, 2004310016 in the real data 
    //  (which might be barely attainable, especially with eager discarding of ineffient paths)
    // another approach might be to from a given node, calculate the hightest value of open nodes (by time to reach them off the total and their total value given that?)

    0

let part2 () =
    0