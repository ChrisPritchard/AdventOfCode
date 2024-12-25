let input = System.IO.File.ReadAllLines "input.txt"

let map =
    input
    |> Array.indexed
    |> Array.collect (fun (y, line) -> Seq.toArray line |> Array.indexed |> Array.map (fun (x, c) -> (x, y), c))
    |> Map.ofArray

let start = Map.tryFindKey (fun _ v -> v = 'S') map |> Option.get
let finish = Map.tryFindKey (fun _ v -> v = 'E') map |> Option.get

let directions = [| -1, 0; 0, -1; 1, 0; 0, 1 |]

let rec pathfinder acc (x, y) =
    let next =
        directions
        |> Array.map (fun (dx, dy) -> x + dx, y + dy)
        |> Array.filter (fun n -> Map.containsKey n map && map[n] <> '#' && not (List.contains n acc))

    if next.Length > 1 then
        failwithf "broken assumption at %A" (x, y)
    else if map[next[0]] = 'E' then
        List.rev (next[0] :: acc)
    else
        pathfinder (next[0] :: acc) next[0]

let path = pathfinder [ start ] start
let set = Set.ofList path
let indexed = path |> Seq.indexed |> Seq.map (fun (i, p) -> p, i) |> Map.ofSeq

let skips =
    path
    |> Seq.collect (fun (x, y) ->
        directions
        |> Array.map (fun (dx, dy) -> (x + dx, y + dy), (x + (2 * dx), y + (2 * dy)))
        |> Array.choose (fun (adj, past) ->
            if not (map.ContainsKey adj) || not (map[adj] = '#') || not (set.Contains past) then
                None
            else
                Some((indexed[past] - indexed[x, y]) - 2)))
    |> Seq.filter (fun n -> n > 0)
    |> Array.ofSeq

printfn "Part 1: %d" (skips |> Array.filter (fun amt -> amt >= 100) |> Array.length)
