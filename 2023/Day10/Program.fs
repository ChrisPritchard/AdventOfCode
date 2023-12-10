let input = Input.value

let grid = input.Split [|'\n'|] |> Array.map (fun s -> s.ToCharArray())

let tile_at (x, y) = grid[y][x]

let mutable sx, sy = -1, 0
while sx = -1 do
    let index = Array.tryFindIndex ((=) 'S') grid[sy]
    match index with
    | None -> sy <- sy + 1
    | Some i -> sx <- i

let tile_types = Map.ofArray [|
        '|', ((0, -1), (0, 1))
        '-', ((-1, 0), (1, 0))
        'F', ((0, 1), (1, 0))
        '7', ((0, 1), (-1, 0))
        'J', ((-1, 0), (0, -1))
        'L', ((1, 0), (0, -1))
    |]

let nx, ny =
    if sy > 0 && grid[sy - 1][sx] = '|' || grid[sy - 1][sx] = 'F' || grid[sy - 1][sx] = '7' then
        sx, sy - 1
    else if sx < grid[0].Length && grid[sy][sx + 1] = '-' || grid[sy][sx + 1] = 'J' || grid[sy][sx + 1] = '7' then
        sx + 1, sy
    else if sy < grid.Length then
        sx, sy + 1
    else
        sx - 1, sy

let add (ax, ay) (bx, by) = ax + bx, ay + by

let rec crawler all_visited step_count last_pos current_pos = 
    let add_current = Set.add current_pos all_visited
    let tile = tile_at current_pos
    if tile = 'S' then add_current, step_count
    else
        let (diff_a, diff_b) = tile_types[tile]
        if add diff_a current_pos = last_pos then 
            crawler add_current (step_count + 1) current_pos (add diff_b current_pos)
        else
            crawler add_current (step_count + 1) current_pos (add diff_a current_pos)

let visited, total_steps = crawler Set.empty 1 (sx, sy) (nx, ny)
let half_way = total_steps / 2

printfn "Part 1: %d" half_way

// for part 2, create a position tester, maybe a bfs. it needs to expand outwards: if it hits an edge then the space its expanding is invalid
// keep expanding until no empty tiles can be reached. if this is invalid then add to the global exclusion list.
// to determine if a space is enclosed, all its edges must be part of the main loop
