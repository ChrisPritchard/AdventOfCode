let input = Input.value

let grid = input.Split [|'\n'|] |> Array.map (fun s -> s.ToCharArray())

let tile_at (x, y) = grid[y][x]

let mutable sx, sy = -1, 0
while sx = -1 do
    let index = Array.tryFindIndex ((=) 'S') grid[sy]
    match index with
    | None -> sy <- sy + 1
    | Some i -> sx <- i


// idea: map each tile type to the possible before and after
// find the next tile, to start crawling from
// for each step of the crawl, track the previous tile position. match the previous position and current type to get next

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

let rec crawler step_count last_pos current_pos = 
    let tile = tile_at current_pos
    if tile = 'S' then step_count
    else
        let (diff_a, diff_b) = tile_types[tile]
        if add diff_a current_pos = last_pos then 
            crawler (step_count + 1) current_pos (add diff_b current_pos)
        else
            crawler (step_count + 1) current_pos (add diff_a current_pos)

let total_steps = crawler 1 (sx, sy) (nx, ny)
let half_way = total_steps / 2

printfn "Part 1: %d" half_way