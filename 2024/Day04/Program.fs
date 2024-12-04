let input = System.IO.File.ReadAllLines "input.txt"

let at x y =
    if x < 0 || x >= input[0].Length || y < 0 || y >= input.Length then
        '.'
    else
        input[y][x]

let make_delta dx dy d (ox, oy) =
    let x = ox + (dx * d)
    let y = oy + (dy * d)

    at x y

let deltas =
    [ make_delta -1 -1
      make_delta 0 -1
      make_delta 1 -1
      make_delta -1 0
      make_delta 1 0
      make_delta -1 1
      make_delta 0 1
      make_delta 1 1 ]

let dir x y delta =
    delta 1 (x, y) = 'A' && delta 2 (x, y) = 'M' && delta 3 (x, y) = 'X'

let check x y =
    deltas |> List.map (dir x y) |> List.filter id |> List.length

let sum =
    input
    |> Array.indexed
    |> Array.sumBy (fun (y, line) ->
        line
        |> Seq.indexed
        |> Seq.filter (fun (_, c) -> c = 'S')
        |> Seq.sumBy (fun (x, _) -> check x y))

printfn "Part 1: %d" sum

let new_check x y =
    let tl = at (x - 1) (y - 1)
    let tr = at (x + 1) (y - 1)
    let bl = at (x - 1) (y + 1)
    let br = at (x + 1) (y + 1)

    ((tl = 'M' && br = 'S') || (tl = 'S' && br = 'M'))
    && ((bl = 'M' && tr = 'S') || (bl = 'S' && tr = 'M'))

let new_sum =
    input
    |> Array.indexed
    |> Array.sumBy (fun (y, line) ->
        line
        |> Seq.indexed
        |> Seq.filter (fun (x, c) -> c = 'A' && new_check x y)
        |> Seq.length)

printfn "Part 2: %d" new_sum
