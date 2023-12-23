let input = System.IO.File.ReadAllLines "input.txt"

// a* for part 1. algo taken from https://en.wikipedia.org/wiki/A*_search_algorithm#Pseudocode

open System
open System.Collections.Generic

let astar<'T when 'T : equality> (isGoal: 'T -> bool) (edges: Dictionary<'T, 'T> -> 'T -> seq<'T>) (d: 'T -> 'T -> float) (h: 'T -> float) (start: 'T) =
    
    let cameFrom = Dictionary<'T, 'T>()
    let gScore = Dictionary<'T, float>()
    gScore.Add (start, 0.)

    let fScore = Dictionary<'T, float>()
    fScore.Add (start, h start)

    let openSet = HashSet<'T>()
    openSet.Add start |> ignore

    let rec reconstructPath acc v =
        if cameFrom.ContainsKey v then
            reconstructPath (v::acc) cameFrom.[v]
        else
            v::acc

    let rec searcher () =
        if openSet.Count = 0 then 
            None
        else
            let current = Seq.minBy (fun v -> if fScore.ContainsKey v then fScore.[v] else Double.MaxValue) openSet
            if isGoal current then Some (reconstructPath [] current)
            else
                openSet.Remove current |> ignore
                let edges = edges cameFrom current
                for neighbour in edges do
                    let tentativeGscore = gScore.[current] + d current neighbour
                    let found, currentGscore = gScore.TryGetValue neighbour
                    if not found || tentativeGscore < currentGscore then
                        cameFrom[neighbour] <- current
                        gScore[neighbour] <- tentativeGscore
                        fScore[neighbour] <- tentativeGscore + h neighbour
                        openSet.Add neighbour |> ignore
                searcher ()

    searcher ()

let distance (x1, y1) (x2, y2): float = 
    sqrt (((float x2 - float x1) ** 2.) + ((float y2 - float y1) ** 2.))
let is_valid (x, y) = 
    x >= 0 && y >= 0 && y < input.Length && x < input[y].Length
let add (x1, y1) (x2, y2) = 
    x1 + x2, y1 + y2
let sub (x1, y1) (x2, y2) = 
    x1 - x2, y1 - y2
let get_val m p = Map.tryFind p m |> Option.defaultValue infinity

let add_sorted to_insert value_to_compare other_valuer list_set = 
    let index_after = list_set |> List.tryFindIndex (fun p -> other_valuer p > value_to_compare)
    match index_after with
    | None | Some 0 -> 
        to_insert::list_set
    | Some index -> 
        List.insertAt index to_insert list_set

let rec reconstruct_path came_from current = 
    let mutable total_path = [current]
    let mutable current = current
    while Map.containsKey current came_from do
        current <- came_from[current]
        total_path <- current::total_path
    total_path

let neighbours point came_from = 
    // custom to challenge: can only turn left, right, or forward
    // and only forward if not already 3 blocks in a row forward
    // to calculate, need to find the last three tiles if possible, and calculate their offsets
    let previous = Map.tryFind point came_from
    let prev_previous = previous |> Option.bind (fun p -> Map.tryFind p came_from)
    let last_three = [Some point; previous; prev_previous] |> List.choose id

    if last_three.Length = 1 then
        [-1,0; 1,0; 0,-1; 0,1] |> List.map (add point) |> List.filter is_valid
    else
        let in_a_line = 
            last_three.Length = 3 && 
            (last_three |> List.map fst |> List.distinct |> List.length = 1
            || last_three |> List.map snd |> List.distinct |> List.length = 1)
        let forward = add point (sub point previous.Value)
        [-1,0; 1,0; 0,-1; 0,1] |> List.map (add point) |> List.filter (fun neighbour -> 
            is_valid neighbour && neighbour <> previous.Value && (not in_a_line || neighbour <> forward))

let neighbours2 (came_from: Dictionary<int * int, int * int>) point = 
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

let a_star (start_x, start_y) (goal_x, goal_y) heuristic d_score neighbours =
    let start = (start_x, start_y)
    let goal = (goal_x, goal_y)

    let mutable open_set = [start]
    let mutable came_from = Map.empty

    let mutable g_score = Map.empty.Add (start, 0.)
    let mutable f_score = Map.empty.Add (start, heuristic start)

    let rec run () =
        match open_set with
        | [] -> None
        | current::remaining ->
            if current = goal then
                Some (reconstruct_path came_from goal)
            else
                open_set <- remaining
                for neighbour in neighbours current came_from do
                    let tentative_gscore = get_val g_score current + d_score neighbour
                    if tentative_gscore < get_val g_score neighbour then
                        came_from <- Map.add neighbour current came_from
                        g_score <- Map.add neighbour tentative_gscore g_score

                        let neighbour_f_score = tentative_gscore + heuristic neighbour
                        f_score <- Map.add neighbour neighbour_f_score f_score
                        if not (List.contains neighbour open_set) then
                            open_set <- add_sorted neighbour neighbour_f_score (get_val f_score) open_set
                run ()
    run ()

let d_score (x, y) = float (input[y][x] - '0')
let goal = input[input.Length - 1].Length - 1, input.Length - 1

//let path = a_star (0, 0) goal (distance goal) d_score neighbours
let path = astar ((=) goal) neighbours2 (fun _ p -> d_score p) (distance goal) (0, 0)
let heat_loss = path.Value |> List.skip 1 |> List.sumBy (d_score >> int)

printfn "%d" heat_loss

for y in 0..input.Length - 1 do
    for x in 0..input[y].Length - 1 do
        if List.contains (x, y) path.Value then printf "#" else printf "%c" (input[y][x])
    printf "\n"