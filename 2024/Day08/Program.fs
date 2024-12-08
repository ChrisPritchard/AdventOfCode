let input = System.IO.File.ReadAllLines "input.txt"

let aereals =
    input
    |> Seq.indexed
    |> Seq.collect (fun (i, line) ->
        line
        |> Seq.indexed
        |> Seq.choose (fun (j, c) -> if c = '.' then None else Some(c, (j, i))))
    |> Seq.groupBy fst
    |> Seq.map (fun (freq, positions) -> freq, positions |> Seq.map snd |> Array.ofSeq)
    |> Array.ofSeq

let pairs =
    aereals
    |> Array.collect (fun (_, positions) ->
        positions
        |> Array.indexed
        |> Array.collect (fun (i, position) ->
            positions
            |> Array.skip (i + 1)
            |> Array.map (fun other_position -> position, other_position)))

let out_of_bounds (x, y) =
    x < 0 || y < 0 || x >= input[0].Length || y >= input.Length

let antinodes =
    pairs
    |> Array.collect (fun ((x1, y1), (x2, y2)) ->
        let dx = x2 - x1
        let dy = y2 - y1
        [| x1 - dx, y1 - dy; x2 + dx, y2 + dy |])
    |> Array.filter (fun p -> not (out_of_bounds p))
    |> Set.ofArray

printfn "Part 1: %d" <| Set.count antinodes

let rec repeater acc (x, y) (dx, dy) =
    let (nx, ny) = x + dx, y + dy

    if out_of_bounds (nx, ny) then
        acc
    else
        let new_acc = Array.append [| nx, ny |] acc
        repeater new_acc (nx, ny) (dx, dy)

let new_antinodes =
    pairs
    |> Array.collect (fun ((x1, y1), (x2, y2)) ->
        let dx = x2 - x1
        let dy = y2 - y1
        let repeats = [| x1, y1 |]
        let down = repeater repeats (x1, y1) (-dx, -dy)
        repeater down (x1, y1) (dx, dy))
    |> Array.filter (fun p -> not (out_of_bounds p))
    |> Set.ofArray

printfn "Part 2: %d" <| Set.count new_antinodes
