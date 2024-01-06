let input = System.IO.File.ReadAllLines "input.txt"

let grid = input |> Array.map (fun s -> s.ToCharArray())

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
    if sy > 0 && (grid[sy - 1][sx] = '|' || grid[sy - 1][sx] = 'F' || grid[sy - 1][sx] = '7') then
        sx, sy - 1
    else if sx < grid[0].Length && (grid[sy][sx + 1] = '-' || grid[sy][sx + 1] = 'J' || grid[sy][sx + 1] = '7') then
        sx + 1, sy
    else if sy < grid.Length then
        sx, sy + 1
    else
        sx - 1, sy

let mutable final_pos = -1, -1

let add (ax, ay) (bx, by) = ax + bx, ay + by

let rec crawler all_visited step_count last_pos current_pos = 
    let add_current = Set.add current_pos all_visited
    let tile = tile_at current_pos
    if tile = 'S' then 
        final_pos <- last_pos
        add_current, step_count
    else
        let (diff_a, diff_b) = tile_types[tile]
        if add diff_a current_pos = last_pos then 
            crawler add_current (step_count + 1) current_pos (add diff_b current_pos)
        else
            crawler add_current (step_count + 1) current_pos (add diff_a current_pos)

let visited, total_steps = crawler Set.empty 1 (sx, sy) (nx, ny)
let half_way = total_steps / 2

printfn "Part 1: %d" half_way

// for part 2 we use even-odd - going along a given x line, every tile NOT on the path is outside|inside|outside|inside and so on, with the switch being vertical edges of the path
// a vertical edge is |, or FJ or L7 (with or without - separating the corners). so in the following, 1|2|3|4|5, 2 and 4 are inside the path with 1, 3 and 5 outside
// same with 1F--J222|3L7444L--75

// need to replace S with its real form so as not to bone the calculations (many failed responses due to this oversight)
let s_real = tile_types |> Map.findKey (fun _ (a, b) -> 
    let ra, rb = add (sx, sy) a, add (sx, sy) b
    (ra = final_pos && rb = (nx, ny)) || (ra = (nx, ny) && rb = final_pos))
grid[sy][sx] <- s_real

// look_for progresses along a line of - until the correct character is found, then returns the position after the end
let rec look_for (x, y) c =
    if x = grid[y].Length then None
    else if grid[y][x] = '-' then
        look_for (x + 1, y) c
    else if grid[y][x] = c then Some (x + 1, y)
    else
        None

// returns the position after the end of the edge if its a valid edge according to rules above
let vertical_edge (x, y) =
    if not (visited.Contains (x, y)) then None
    else
        let tile = grid[y][x]
        if tile = '|' then Some (x + 1, y)
        else if tile = 'F' then
            look_for (x + 1, y) 'J'
        else if tile = 'L' then
            look_for (x + 1, y) '7'
        else
            None

// go through each line, perform even-odd calculations to find inside and outside points
let mutable inside_count = 0
for y in 0..grid.Length - 1 do
    let mutable intersections = 0
    let mutable x = 0
    while x < grid[0].Length do
        match vertical_edge (x, y) with
        | Some (nx, _) -> 
            intersections <- intersections + 1
            x <- nx
        | None ->
            if intersections % 2 <> 0 && not (visited.Contains (x, y)) then
                inside_count <- inside_count + 1
            x <- x + 1

printfn "Part 2: %d" inside_count