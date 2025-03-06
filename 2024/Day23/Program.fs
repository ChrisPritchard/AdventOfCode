let input = System.IO.File.ReadAllLines "input.txt"

let connections =
    input
    |> Array.collect (fun s -> let p = s.Split "-" in [| p[0], p[1]; p[1], p[0] |])
    |> Array.groupBy fst
    |> Array.map (fun (k, v) -> k, Array.map snd v)

let as_map =
    connections |> Array.map (fun (k, v) -> k, Set.ofArray v) |> Map.ofArray

let t_comp_sets =
    connections
    |> Array.filter (fun (k, _) -> k[0] = 't')
    |> Array.collect (fun (k, v) ->
        v
        |> Array.collect (fun other ->
            v
            |> Array.filter (fun other2 -> other2 <> other && as_map[other].Contains other2)
            |> Array.map (fun other2 -> Array.sort [| k; other; other2 |])))
    |> Array.distinct
    |> Array.length

printfn "Part 1: %d" t_comp_sets
