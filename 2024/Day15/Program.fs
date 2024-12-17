let input = System.IO.File.ReadAllLines "input.txt"

let map_break = input |> Array.findIndex (fun line -> line.Trim() = "")

let mutable map = input[0 .. map_break - 1] |> Array.map Seq.toArray

let instructions = input[map_break + 1 ..] |> Array.collect Seq.toArray

let find_start () =
    map
    |> Seq.indexed
    |> Seq.pick (fun (y, line) ->
        line
        |> Seq.indexed
        |> Seq.tryPick (fun (x, c) -> if c = '@' then Some(x, y) else None))

let directions = [| 0, -1; 1, 0; 0, 1; -1, 0 |]

let next (x, y) dir =
    let (dx, dy) =
        match dir with
        | '^' -> directions[0]
        | '>' -> directions[1]
        | 'v' -> directions[2]
        | _ -> directions[3]

    x + dx, y + dy

let oob (x, y) =
    x < 0 || y < 0 || x = map[0].Length || y = map.Length

// two steps: verify can move, chaining forward, then move chaining forward
// move only works if next can mve

let rec can_move pos dir =
    let (nx, ny) = next pos dir

    if oob (nx, ny) || map[ny][nx] = '#' then
        false
    else if map[ny][nx] = '.' then
        true
    else if dir = '<' || dir = '>' then
        can_move (nx, ny) dir
    else if map[ny][nx] <> '[' && map[ny][nx] <> ']' then
        can_move (nx, ny) dir
    else if map[ny][nx] = '[' then
        can_move (nx, ny) dir && can_move (nx + 1, ny) dir
    else
        can_move (nx, ny) dir && can_move (nx - 1, ny) dir

let rec move (x, y) dir =
    if map[y][x] = '.' then
        ()
    else if dir <> '<' && dir <> '>' && (map[y][x] = '[' || map[y][x] = ']') then
        let offset = if map[y][x] = '[' then 1 else -1
        let (nx1, ny1) = next (x, y) dir
        move (nx1, ny1) dir
        map[ny1][nx1] <- map[y][x]
        map[y][x] <- '.'

        let (nx2, ny2) = next (x + offset, y) dir
        move (nx2, ny2) dir
        map[ny2][nx2] <- map[y][x + offset]
        map[y][x + offset] <- '.'
    else
        let (nx, ny) = next (x, y) dir
        move (nx, ny) dir
        map[ny][nx] <- map[y][x]
        map[y][x] <- '.'

let try_move pos dir =
    if not (can_move pos dir) then
        pos
    else
        move pos dir
        next pos dir

let gps_score () =
    map
    |> Array.indexed
    |> Array.sumBy (fun (y, line) ->
        line
        |> Array.indexed
        |> Array.sumBy (fun (x, c) -> if c <> 'O' && c <> '[' then 0 else y * 100 + x))

let mutable robot = find_start ()

for dir in instructions do
    robot <- try_move robot dir

printfn "Part 1: %d" (gps_score ())

map <-
    input[0 .. map_break - 1]
    |> Array.map (fun line ->
        line
        |> Seq.collect (fun c ->
            match c with
            | '#' -> "##"
            | '.' -> ".."
            | 'O' -> "[]"
            | _ -> "@.")
        |> Seq.toArray)

robot <- find_start ()

for dir in instructions do
    robot <- try_move robot dir

// printfn "%c" dir

// for line in map do
//     for c in line do
//         printf "%c" c

//     printfn ""

printfn "Part 2: %d" (gps_score ())
