module Day12

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

let part1And2 () =
    let map = readEmbedded "day12"
    let s, e = 
        (((0, 0), (0, 0)), [0..(map.Length-1)]) 
        ||> Seq.fold (fun (start, target) y ->
            ((start, target), [0..(map[y].Length-1)])
            ||> Seq.fold (fun (start, target) x ->
                if map[y][x] = 'S' then (x, y), target
                else if map[y][x] = 'E' then start, (x, y)
                else start, target))

    let height x y =
        if (x, y) = s then 0 else if (x, y) = e then 25 else (int map.[y].[x]) - int 'a'

    let edges (x, y) = 
        seq {
            if x > 0 && height (x-1) y <= height x y + 1 then yield x - 1, y
            if y > 0 && height x (y-1) <= height x y + 1 then yield x, y - 1
            if x < map[y].Length - 1 && height (x+1) y <= height x y + 1 then yield x + 1, y
            if y < map.Length - 1 && height x (y+1) <= height x y + 1 then yield x, y + 1
        }
    
    let part1 = 
        match bfs ((=) e) edges s with Some p -> Seq.length p - 1 | _ -> failwith "no path found"
    let part2 = 
        map 
        |> Seq.indexed 
        |> Seq.collect (fun (y, line) -> 
            line 
            |> Seq.indexed 
            |> Seq.filter (snd >> (=) 'a') 
            |> Seq.choose (fun (x, _) -> 
                bfs ((=) e) edges (x, y) |> Option.map (fun p -> Seq.length p - 1)))
        |> Seq.min

    part1, part2
    