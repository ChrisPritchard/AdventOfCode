let input = System.IO.File.ReadAllLines "input.txt"

let starting_position =
    input
    |> Array.indexed
    |> Array.pick (fun (i, line) ->
        line
        |> Seq.indexed
        |> Seq.tryPick (fun (j, c) -> if c = '^' then Some(j, i) else None))

let rec march acc (x, y) dir =
    let (dx, dy) =
        match dir with
        | 0 -> 0, -1
        | 1 -> 1, 0
        | 2 -> 0, 1
        | _ -> -1, 0

    let nx, ny = x + dx, y + dy

    if nx < 0 || nx = input[0].Length || ny < 0 || ny = input.Length then
        acc
    else
        match input[ny][nx] with
        | '.'
        | '^' -> march (Set.add (nx, ny) acc) (nx, ny) dir
        | _ -> march acc (x, y) (if dir = 3 then 0 else dir + 1)

let path = march (Set.empty.Add starting_position) starting_position 0
let sum = Set.count path

printfn "Part 1: %d" sum

// for part 2, calculate a vector at each point and also march a line in the turn direction
// if the marched line will hit and turn to match an existing vector, we have a loop

let rec marcher (x, y) dir =
    let (dx, dy) =
        match dir with
        | 0 -> 0, -1
        | 1 -> 1, 0
        | 2 -> 0, 1
        | _ -> -1, 0

    let nx, ny = x + dx, y + dy

    if nx < 0 || nx = input[0].Length || ny < 0 || ny = input.Length then
        None
    else
        match input[ny][nx] with
        | '.'
        | '^' -> marcher (nx, ny) dir
        | _ -> Some((nx, ny), if dir = 3 then 0 else dir + 1)

let rec check_turn (x, y) dir =
    marcher (x, y) (if dir = 3 then 0 else dir + 1)

let rec new_march loops (acc: Set<(int * int) * int>) (x, y) dir =
    let (dx, dy) =
        match dir with
        | 0 -> 0, -1
        | 1 -> 1, 0
        | 2 -> 0, 1
        | _ -> -1, 0

    let nx, ny = x + dx, y + dy

    if nx < 0 || nx = input[0].Length || ny < 0 || ny = input.Length then
        loops
    else
        match input[ny][nx] with
        | '.'
        | '^' ->
            let new_loops =
                match check_turn (nx, ny) dir with
                | Some vector when acc.Contains vector -> loops + 1
                | _ -> loops

            new_march new_loops (Set.add ((nx, ny), dir) acc) (nx, ny) dir
        | _ -> new_march loops (Set.add ((nx, ny), dir) acc) (x, y) (if dir = 3 then 0 else dir + 1)

let loop_possibles =
    new_march 0 (Set.empty.Add(starting_position, 0)) starting_position 0

printfn "Part 2: %d" loop_possibles
