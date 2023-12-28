let input = System.IO.File.ReadAllLines "input.txt"

// key to this challenge is the neighbour algorithm
// and key to that is not just storing score and queue by position, but by position and direction
// otherwise https://en.wikipedia.org/wiki/Dijkstra%27s_algorithm#Pseudocode

// change is in neighbours processing. rather than go through adjacent positions to a given point,
// we need to calculate the points along a line from that point. e.g. 321 - x 123. These points would be 
// horizontal in this case, and each would be marked as the previous of the next if a score is better (lower)

let djikstra neighbours edge start is_goal = 
    let mutable distances = Map.empty.Add (start, 0.)
    let mutable previous = Map.empty
    let dist v = Map.tryFind v distances |> Option.defaultValue infinity
    let mutable Q = [start]

    let add_to_Q v = 
        let index_after = Q |> List.tryFindIndex (fun p -> dist p > dist v)
        match index_after with
        | None -> Q <- List.append Q [v]
        | Some index -> Q <- List.insertAt index v Q

    let rec reconstruct_path current acc = 
        let new_acc = current::acc
        if previous.ContainsKey current then
            reconstruct_path (previous[current]) new_acc
        else
            Some new_acc
    
    let rec proceed () =
        match Q with
        | [] -> None
        | u::rem ->
            if is_goal u then 
                reconstruct_path u []
            else
                Q <- rem
                let mutable cont = true
                for (v, prev) in neighbours u do
                    if cont then
                        let alt = dist u + edge u v
                        if alt < dist v then
                            distances <- distances.Add (v, alt)
                            previous <- previous.Add (v, prev)
                            add_to_Q v
                        else
                            cont <- false
                proceed ()
    proceed ()

// flaw - previous should only be set for complete paths where alt is set

type Direction = Vertical | Horizontal | Start
let valid_directions dir = 
    match dir with
    | Vertical -> [-1,0; 1,0]
    | Horizontal -> [0,-1; 0,1]
    | Start -> [-1,0; 1,0; 0,-1; 0,1]
let dir_from (dx, dy) = if dx = 0 then Vertical else Horizontal
let is_valid (x, y) = x >= 0 && y >= 0 && y < input.Length && x < input[y].Length

let neighbours min_size max_size (x, y, dir) = 
    let directions = valid_directions dir
    seq {
        for (dx, dy) in directions do
            let mutable prev = (x, y, dir)
            let new_dir = dir_from (dx, dy)
            for size in 1..max_size do 
                let nx, ny = x + (dx * size), y + (dy * size)
                if is_valid (nx, ny) then
                    yield (nx, ny, new_dir), prev
                    prev <- (nx, ny, new_dir)
    }

let start = 0, 0, Start
let goal = input[input.Length - 1].Length - 1, input.Length - 1
let is_goal (x, y, _) = (x, y) = goal
let m (x, y) = int (input[y][x]) - int '0'
let edge (sx, sy, _) (tx, ty, _) = 
    if tx <> sx then
        if sx > tx then
            [tx..sx-1] |> List.sumBy (fun ox -> m (ox, ty)) |> float
        else
            [sx+1..tx] |> List.sumBy (fun ox -> m (ox, ty))
    else
        if sy > ty then
            [ty..sy-1] |> List.sumBy (fun oy -> m (tx, oy))
        else
            [sy+1..ty] |> List.sumBy (fun oy -> m (tx, oy))

let path = djikstra (neighbours 1 3) edge start is_goal
printfn "%A" path

for y in 0..input.Length - 1 do
    for x in 0..input[y].Length - 1 do
        match List.tryFind (fun (ox, oy, _) -> x = ox && y = oy) path.Value with
        | Some (_, _, Horizontal) -> printf "-"
        | Some (_, _, Vertical) -> printf "|"
        | _ -> printf "%d" (m (x, y))
    printf "\n"

let part1 = path.Value |> List.sumBy (fun (x, y, _) -> m (x, y))
printfn "%d" part1

