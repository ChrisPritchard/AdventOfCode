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
