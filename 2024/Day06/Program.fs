let input = System.IO.File.ReadAllLines "input.txt"

let starting_position =
    input
    |> Array.indexed
    |> Array.pick (fun (i, line) ->
        line
        |> Seq.indexed
        |> Seq.tryPick (fun (j, c) -> if c = '^' then Some(j, i, 0) else None))

let mutable blocks =
    input |> Array.map (fun line -> line |> Seq.map ((=) '#') |> Array.ofSeq)

let find_turn (x, y, dir) =
    if dir = 0 then
        [| (y - 1) .. (-1) .. 0 |]
        |> Array.tryPick (fun i -> if blocks[i][x] then Some(x, i + 1, 1) else None)
    else if dir = 1 then
        [| (x + 1) .. input[0].Length - 1 |]
        |> Array.tryPick (fun i -> if blocks[y][i] then Some(i - 1, y, 2) else None)
    else if dir = 2 then
        [| (y + 1) .. input.Length - 1 |]
        |> Array.tryPick (fun i -> if blocks[i][x] then Some(x, i - 1, 3) else None)
    else
        [| (x - 1) .. (-1) .. 0 |]
        |> Array.tryPick (fun i -> if blocks[y][i] then Some(i + 1, y, 0) else None)

let rec find_path visited turns pos =
    match find_turn pos with
    | None -> Some(List.rev turns)
    | Some t when Set.contains t visited -> None
    | Some t ->
        let new_visited = Set.add t visited
        find_path new_visited (t :: turns) t

let all_turns = find_path Set.empty [ starting_position ] starting_position

let delta =
    function
    | (_, _, 0) -> 0, -1
    | (_, _, 1) -> 1, 0
    | (_, _, 2) -> 0, 1
    | _ -> -1, 0

let points_between (x, y, dir) (nx, ny, _) =
    let dx, dy =
        match dir with
        | 0 -> 0, -1
        | 1 -> 1, 0
        | 2 -> 0, 1
        | _ -> -1, 0

    let is_out_of_bounds (x, y) =
        x < 0 || y < 0 || x = input[0].Length || y = input.Length

    let rec counter acc (x, y) =
        let next = x + dx, y + dy

        if next = (nx, ny) || is_out_of_bounds next then
            Set.ofList acc
        else
            counter (next :: acc) next

    counter [ x, y ] (x, y)

let rec find_all_points acc pos turns =
    match turns with
    | [] -> points_between pos (-1, -1, -1) |> Set.union acc
    | n :: rest ->
        let next = points_between pos n |> Set.union acc
        find_all_points next n rest

let all_points =
    find_all_points Set.empty starting_position (List.skip 1 all_turns.Value)

printfn "Part 1: %A" (Set.count all_points)

let mutable loops = 0

let (sx, sy, _) = starting_position

for point in all_points do
    if point <> (sx, sy) then
        let (x, y) = point
        blocks[y][x] <- true

        match find_path Set.empty [ starting_position ] starting_position with
        | None -> loops <- loops + 1
        | Some _ -> ()

        blocks[y][x] <- false

printfn "Part 2: %d" loops
