module Day24

open Common
open System.Collections.Generic

let input = readEmbedded "day24"
let start_tile = ((Seq.findIndex ((=) '.') input[0]), 0)
let end_tile = ((Seq.findIndex ((=) '.') input[input.Length - 1]), input.Length - 1)

let x_out = input[0].Length - 1
let y_out = input.Length - 1

let start_storms = 
    input 
    |> Array.indexed 
    |> Seq.collect (fun (y, line) -> 
        line 
        |> Seq.indexed 
        |> Seq.choose (fun (x, c) -> 
            if c = '.' || c = '#' then None 
            else Some (x, y, c))
        )
    |> Set.ofSeq

let next_storms current = 
    current 
    |> Set.toSeq
    |> Seq.map (fun (x, y, d) ->
        let nx, ny = 
            match d with
            | '^' -> x, y - 1
            | 'v' -> x, y + 1
            | '<' -> x - 1, y
            | '>' | _ -> x + 1, y
        if nx = 0 then x_out - 1, y, d
        else if nx = x_out then 1, y, d
        else if ny = 0 then x, y_out - 1, d
        else if ny = y_out then x, 1, d
        else nx, ny, d)
    |> Set.ofSeq

let rec climate acc current =
    let new_storms = next_storms current
    if Set.difference new_storms start_storms |> Set.isEmpty then
        List.rev acc |> Array.ofList
    else
        climate (new_storms::acc) new_storms

let storm_states = climate [start_storms] start_storms
let all_storms = storm_states |> Array.map (Set.map (fun (x, y, _) -> (x, y)))

let edges step (x, y) = 
    let storms = all_storms[(step + 1) % all_storms.Length]
    [|0,0;1,0;0,1;-1,0;0,-1|] 
    |> Array.map (fun (dx, dy) -> x + dx, y + dy) 
    |> Array.filter (fun (x, y) -> 
        if (x, y) = start_tile || (x, y) = end_tile then true
        else
            x > 0 && x < x_out && y > 0 && y < y_out &&
            not (Set.contains (x, y) storms))

let bfs isGoal edges start =
    let queue = Queue<(int * int) * int>()
    let discovered = HashSet<(int * int) * int>()
    let parents = Dictionary<(int * int) * int, (int * int) * int>()
    
    discovered.Add (start, 0) |> ignore
    queue.Enqueue (start, 0)

    let rec reconstructPath acc (v, s) =
        if parents.ContainsKey (v, s) then
            reconstructPath (v::acc) parents.[(v, s)]
        else
            v::acc

    let rec searcher () =
        if queue.Count = 0 then None
        else
            let (v, step) = queue.Dequeue ()
            if isGoal v then
                reconstructPath [] (v, step) |> Some
            else
                let edges = edges step v
                for w in edges do
                    if not (discovered.Contains (w, step + 1)) then
                        discovered.Add (w, step + 1) |> ignore
                        parents.Add ((w, step + 1), (v, step))
                        queue.Enqueue (w, step + 1)
                searcher ()
            
    searcher ()

let part1 () =

    let path = bfs ((=) end_tile) edges start_tile
    match path with
    | Some p -> (List.length p) - 1
    | _ -> failwith "no path found"

let part2 () =
    0
