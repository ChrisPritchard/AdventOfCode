let input = Input.value

let grid = input.Split [|'\n'|]

let mutable illuminated = Set.empty

type Dir = North | East | South | West

let rec light_beam x y dir =
    let nx, ny = 
        match dir with
        | North -> x, y - 1
        | East -> x + 1, y
        | South -> x, y + 1
        | West -> x - 1, y
    if nx < 0 || ny < 0 || ny = grid.Length || nx = grid[ny].Length || Set.contains (nx, ny, dir) illuminated then ()
    else
        illuminated <- Set.add (nx, ny, dir) illuminated
        match grid[ny][nx], dir with
        | '|', West | '|', East -> 
            light_beam nx ny North
            light_beam nx ny South
        | '-', North | '-', South ->
            light_beam nx ny East
            light_beam nx ny West
        | '\\', East ->
            light_beam nx ny South
        | '\\', North ->
            light_beam nx ny West
        | '\\', West ->
            light_beam nx ny North
        | '\\', South ->
            light_beam nx ny East
        | '/', East ->
            light_beam nx ny North
        | '/', North ->
            light_beam nx ny East
        | '/', West ->
            light_beam nx ny South
        | '/', South ->
            light_beam nx ny West
        | _ -> 
            light_beam nx ny dir

light_beam -1 0 East

let just_illuminated = Set.map (fun (x, y, dir) -> x, y) illuminated
let part1 = just_illuminated |> Set.count

// for i in 0..grid.Length-1 do
//     for j in 0..grid[i].Length-1 do
//         if Set.contains (j, i) just_illuminated then printf "#"
//         else printf "%c" (grid[i][j])
//     printf "\n"

printfn "Part 1: %d" part1