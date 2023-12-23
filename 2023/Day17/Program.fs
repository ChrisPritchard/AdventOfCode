let input = System.IO.File.ReadAllLines "input.txt"

// a* for part 1. algo taken from https://en.wikipedia.org/wiki/A*_search_algorithm#Pseudocode

open System
open System.Collections.Generic

let djikstra<'T when 'T : equality> (vertices: 'T seq) (target: 'T -> bool) (edges: Dictionary<'T, 'T> -> 'T -> seq<'T>) (edge: 'T -> 'T -> float) (source: 'T) =
    
    let prev = Dictionary<'T, 'T>()
    let dist = Dictionary<'T, float>()
    let Q = HashSet<'T>()
    for v in vertices do 
        dist[v] <- infinity
        Q.Add v |> ignore
    dist[source] <- 0.

    let rec searcher () = 
        if Q.Count = 0 then None
        else
            let u = Seq.minBy (fun v -> dist[v]) Q

            if target u then
                let mutable S = [u]
                let mutable u = u
                while prev.ContainsKey u do
                    u <- prev[u]
                    S <- u::S
                Some S
            else
                Q.Remove u |> ignore

                for v in edges prev u |> Seq.filter (fun v -> Q.Contains v) do
                    let alt = dist[u] + edge u v
                    if alt < dist[v] then
                        dist[v] <- alt
                        prev[v] <- u
                searcher()
    searcher ()

let is_valid (x, y) = 
    x >= 0 && y >= 0 && y < input.Length && x < input[y].Length
let add (x1, y1) (x2, y2) = 
    x1 + x2, y1 + y2
let sub (x1, y1) (x2, y2) = 
    x1 - x2, y1 - y2

let last_four_points (came_from: Dictionary<int * int, int * int>) current = 
    // custom to challenge: can only turn left, right, or forward
    // and only forward if not already 3 blocks in a row forward
    // to calculate, need to find the last three tiles if possible, and calculate their offsets
    let previous = if came_from.ContainsKey current then Some came_from[current] else None
    let prev_previous = previous |> Option.bind (fun p -> if came_from.ContainsKey p then Some came_from[p] else None)
    let prev_prev_previous = prev_previous |> Option.bind (fun p -> if came_from.ContainsKey p then Some came_from[p] else None)
    [Some current; previous; prev_previous; prev_prev_previous] |> List.choose id

let neighbours (last_four: (int * int) list) =
    let point = last_four[0]
    if last_four.Length = 1 then
        [-1,0; 1,0; 0,-1; 0,1] |> List.map (add point) |> List.filter is_valid |> Seq.ofList
    else
        let previous = last_four[1]
        let in_a_line = 
            last_four.Length = 4 && 
            (last_four |> List.map fst |> List.distinct |> List.length = 1
            || last_four |> List.map snd |> List.distinct |> List.length = 1)
        let forward = add point (sub point previous)
        [-1,0; 1,0; 0,-1; 0,1] |> List.map (add point) |> List.filter (fun neighbour -> 
            is_valid neighbour && neighbour <> previous && (not in_a_line || neighbour <> forward)) |> Seq.ofList

let d_score (x, y) = float (input[y][x] - '0')
let all_points = [|0..input.Length-1|] |> Array.collect (fun y -> [|0..input[y].Length-1|] |> Array.map (fun x -> x, y))
let goal = input[input.Length - 1].Length - 1, input.Length - 1

let path = djikstra all_points ((=) goal) (fun a p -> last_four_points a p |> neighbours) (fun _ p -> d_score p) (0, 0)
let heat_loss = path.Value |> List.skip 1 |> List.sumBy (d_score >> int)

printfn "%d" heat_loss

for y in 0..input.Length - 1 do
    for x in 0..input[y].Length - 1 do
        if List.contains (x, y) path.Value then printf "#" else printf "%c" (input[y][x])
    printf "\n"
