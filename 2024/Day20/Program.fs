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

let path = pathfinder [ start ] start |> Array.ofList
let path_set = Set.ofArray path
let path_indexed = path |> Seq.indexed |> Seq.map (fun (i, p) -> p, i) |> Map.ofSeq

let within_two min (x, y) =
    directions
    |> Array.map (fun (dx, dy) -> (x + dx, y + dy), (x + 2 * dx, y + 2 * dy))
    |> Array.where (fun (adj, past) -> map.ContainsKey adj && map[adj] = '#' && path_set.Contains past)
    |> Array.map (fun (_, past) -> path_indexed[past] - path_indexed[x, y] - 2)
    |> Array.where (fun n -> n >= min)
    |> Array.length

printfn "Part 1: %d" (Seq.sumBy (within_two 100) path)

let all_possibles =
    [| -20 .. 20 |]
    |> Array.collect (fun dy ->
        [| -20 .. 20 |]
        |> Array.where (fun dx -> abs dx + abs dy <= 20)
        |> Array.map (fun dx -> dx, dy))

let dist (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)

let within_twenty min (x, y) =
    all_possibles
    |> Array.map (fun (dx, dy) -> (x + dx, y + dy))
    |> Array.where (fun target -> path_set.Contains target)
    |> Array.map (fun target -> path_indexed[target] - path_indexed[x, y] - dist target (x, y))
    |> Array.where (fun n -> n >= min)
    |> Array.length

printfn "Part 2: %d" (Seq.sumBy (within_twenty 100) path)
