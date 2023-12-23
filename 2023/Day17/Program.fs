let input = System.IO.File.ReadAllLines "input.txt"

// a* for part 1. algo taken from https://en.wikipedia.org/wiki/A*_search_algorithm#Pseudocode

open System
open System.Collections.Generic

let djikstra<'T when 'T : equality> (all_points: 'T seq) (isGoal: 'T -> bool) (edges: Dictionary<'T, 'T> -> 'T -> seq<'T>) (d: 'T -> 'T -> float) (start: 'T) =
    
    let previous = Dictionary<'T, 'T>()
    let distances = Dictionary<'T, float>()
    let queue = HashSet<'T>()
    for point in all_points do 
        distances[point] <- infinity
        queue.Add point |> ignore
    distances[start] <- 0.

    let rec searcher () = 
        if queue.Count = 0 then None
        else
            let u = Seq.minBy (fun v -> distances[v]) queue

            if isGoal u then
                let mutable total_path = [u]
                let mutable current = u
                while previous.ContainsKey current do
                    current <- previous[current]
                    total_path <- current::total_path
                Some total_path
            else
                queue.Remove u |> ignore

                let neighbours = edges previous u
                for v in neighbours |> Seq.filter (fun v -> queue.Contains v) do
                    let alt = distances[u] + d u v
                    if alt < distances[v] then
                        distances[v] <- alt
                        previous[v] <- u
                searcher()
    searcher ()

let is_valid (x, y) = 
    x >= 0 && y >= 0 && y < input.Length && x < input[y].Length
let add (x1, y1) (x2, y2) = 
    x1 + x2, y1 + y2
let sub (x1, y1) (x2, y2) = 
    x1 - x2, y1 - y2

let neighbours (came_from: Dictionary<int * int, int * int>) point = 
    // custom to challenge: can only turn left, right, or forward
    // and only forward if not already 3 blocks in a row forward
    // to calculate, need to find the last three tiles if possible, and calculate their offsets
    let previous = if came_from.ContainsKey point then Some came_from[point] else None
    let prev_previous = previous |> Option.bind (fun p -> if came_from.ContainsKey p then Some came_from[p] else None)
    let last_three = [Some point; previous; prev_previous] |> List.choose id

    if last_three.Length = 1 then
        [-1,0; 1,0; 0,-1; 0,1] |> List.map (add point) |> List.filter is_valid |> Seq.ofList
    else
        let in_a_line = 
            last_three.Length = 3 && 
            (last_three |> List.map fst |> List.distinct |> List.length = 1
            || last_three |> List.map snd |> List.distinct |> List.length = 1)
        let forward = add point (sub point previous.Value)
        [-1,0; 1,0; 0,-1; 0,1] |> List.map (add point) |> List.filter (fun neighbour -> 
            is_valid neighbour && neighbour <> previous.Value && (not in_a_line || neighbour <> forward)) |> Seq.ofList

let d_score (x, y) = float (input[y][x] - '0')
let all_points = [|0..input.Length-1|] |> Array.collect (fun y -> [|0..input[y].Length-1|] |> Array.map (fun x -> x, y))
let goal = input[input.Length - 1].Length - 1, input.Length - 1

//let path = a_star (0, 0) goal (distance goal) d_score neighbours
let path = djikstra all_points ((=) goal) neighbours (fun _ p -> d_score p) (0, 0)
let heat_loss = path.Value |> List.sumBy (d_score >> int)

printfn "%d" heat_loss

for y in 0..input.Length - 1 do
    for x in 0..input[y].Length - 1 do
        if List.contains (x, y) path.Value then printf "#" else printf "%c" (input[y][x])
    printf "\n"