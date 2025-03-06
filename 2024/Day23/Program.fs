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

open System.Collections.Generic

let sets = List<HashSet<string>>()

for connection in input do
    let a, b = let parts = connection.Split "-" in parts[0], parts[1]

    match sets |> Seq.tryFind (fun s -> s.Contains a || s.Contains b) with
    | None ->
        let new_set = HashSet<string>()
        let _ = new_set.Add a
        let _ = new_set.Add b
        sets.Add new_set
    | Some s ->
        let _ = s.Add a
        let _ = s.Add b
        ()

let password =
    sets
    |> Seq.filter (fun s ->
        let all_comps = Seq.toArray s

        all_comps
        |> Array.forall (fun comp ->
            all_comps
            |> Array.filter (fun other -> other <> comp)
            |> Array.forall (fun other -> as_map[comp].Contains other)))
    |> Seq.sortByDescending (fun s -> s.Count)
    |> Seq.head
    |> Seq.sort
    |> String.concat ","

printfn "Part 2: %s" password
