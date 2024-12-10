let input = System.IO.File.ReadAllLines "input.txt"

let map =
    input
    |> Array.indexed
    |> Array.collect (fun (y, line) ->
        line
        |> Seq.indexed
        |> Seq.map (fun (x, c) -> (x, y), int c - int '0')
        |> Seq.toArray)
    |> Map.ofArray

let directions = [| -1, 0; 0, -1; 1, 0; 0, 1 |]

let find_trails (x, y) =
    let rec expand n (x, y) =
        if n = 9 then
            [| (x, y) |]
        else
            directions
            |> Array.choose (fun (dx, dy) ->
                let pos = x + dx, y + dy

                if Map.containsKey pos map && map[pos] = n + 1 then
                    Some(expand (n + 1) pos)
                else
                    None)
            |> Array.collect id

    expand 0 (x, y)

let trail_heads = map |> Map.filter (fun _ v -> v = 0) |> Map.keys |> Seq.toArray

let sum =
    trail_heads
    |> Seq.sumBy (fun p -> find_trails p |> Array.distinct |> Array.length)

printfn "Part 1: %d" sum

let new_sum = trail_heads |> Seq.sumBy (fun p -> find_trails p |> Array.length)

printfn "Part 2: %d" new_sum
