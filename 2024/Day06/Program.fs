let input = System.IO.File.ReadAllLines "input.txt"

let starting_position =
    input
    |> Array.indexed
    |> Array.pick (fun (i, line) ->
        line
        |> Seq.indexed
        |> Seq.tryPick (fun (j, c) -> if c = '^' then Some(j, i) else None))

let delta =
    function
    | 0 -> 0, -1
    | 1 -> 1, 0
    | 2 -> 0, 1
    | _ -> -1, 0

let turn dir = if dir = 3 then 0 else dir + 1



// revised version: starting position. calculate next
// if blocked, then turn and recheck
// else add vector and calculate next
// feels a bit ass backward, but will ensure each coords are only added once

type Pathing =
    | Normal of ((int * int) * int) list
    | Loop

let rec march block_checker acc (x, y) dir =

    let (dx, dy) = delta dir
    let nx, ny = x + dx, y + dy

    if nx < 0 || nx = input[0].Length || ny < 0 || ny = input.Length then
        Normal(((x, y), dir) :: acc)
    else if List.contains ((nx, ny), dir) acc then
        Loop
    else if block_checker (nx, ny) then
        march block_checker acc (x, y) (turn dir)
    else
        march block_checker (((x, y), dir) :: acc) (nx, ny) dir

let blocked (x, y) =
    let c = input[y][x] in c <> '.' && c <> '^'

match march blocked [] starting_position 0 with
| Normal path ->
    let sum = List.map fst path |> Set.ofList |> Set.count
    printfn "Part 1: %d" sum
| _ -> printfn "Part 1 failed: hit a loop"

// for ((x, y), dir) in path do
//     printfn
//         "%d %d %s"
//         x
//         y
//         (match dir with
//          | 0 -> "up"
//          | 1 -> "right"
//          | 2 -> "down"
//          | _ -> "left")
