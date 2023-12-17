let input = Input.value

let grid = input.Split [|'\n'|]

type Dir = North | East | South | West

let next_dirs (c: char) dir = 
    match c, dir with
    | '|', West | '|', East -> [North;South]
    | '-', North | '-', South -> [East;West]
    | '\\', East ->[South]
    | '\\', South -> [East]
    | '\\', North -> [West]
    | '\\', West -> [North]
    | '/', East -> [North]
    | '/', North -> [East]
    | '/', West -> [South]
    | '/', South -> [West]
    | _ ->  [dir]

let rec light_beam queue beams =
    match queue with
    | (x, y, dir)::rem ->
        if Set.contains (x, y, dir) beams || y < 0 || x < 0 || y = grid.Length || x = grid[y].Length then
            light_beam rem beams
        else
            let new_illuminated = Set.add (x, y, dir) beams
            let new_tiles = next_dirs (grid[y][x]) dir |> List.map (fun d -> 
                match d with
                | North -> x, y - 1, d
                | West -> x - 1, y, d
                | South ->  x, y + 1, d
                | East ->  x + 1, y, d)
            let new_queue = List.append new_tiles rem
            light_beam new_queue new_illuminated
    | [] -> beams

let beams = light_beam [0, 0, East] Set.empty
let just_illuminated = Set.map (fun (x, y, dir) -> x, y) beams
let part1 = just_illuminated |> Set.count

printfn "Part 1: %d" part1