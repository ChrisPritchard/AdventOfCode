module Day23

open Common

type Dir = North | South | West | East

let add (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

let dirs = [|North; South; West; East|]

let sort_by start_dir =
    match start_dir % 4 with
    | 0 -> dirs
    | 1 -> [|South; West; East; North|]
    | 2 -> [|West; East; North; South|]
    | 3 | _ -> [|East; North; South; West|]

let diff (x, y) dir =
    match dir with
    | North -> [|add (x, y) (-1, -1);add (x, y) (0, -1);add (x, y) (1, -1)|]
    | South -> [|add (x, y) (-1, 1);add (x, y) (0, 1);add (x, y) (1, 1)|]
    | West -> [|add (x, y) (-1, -1);add (x, y) (-1, 0);add (x, y) (-1, 1)|]
    | East -> [|add (x, y) (1, -1);add (x, y) (1, 0);add (x, y) (1, 1)|]

let next_place p existing start_dir (no_move, new_move, blocked) =
    let no_adjacent = dirs |> Array.collect (diff p) |> Array.forall (fun o -> not (Set.contains o existing))
    if no_adjacent then (Set.add p no_move), new_move, blocked
    else
        let next = sort_by start_dir |> Array.tryFind (fun d -> diff p |> Array.forall (fun o -> not (Set.contains o existing)))
        match next with
        | None -> (Set.add p no_move), new_move, blocked
        | Some p when Set.contains p new_move -> Set.add

let is_available (x, y) dir existing =
    Array.forall (fun p -> not (Set.contains p existing)) (diff (x, y) dir)

let parse_elves () =
    let input = readEmbedded "day23"
    input |> Array.indexed |> Array.fold (fun acc (y, line) ->
        line |> Seq.indexed |> Seq.fold (fun acc (x, char) -> 
            if char <> '#' then acc
            else Set.add (x, y) acc) acc) Set.empty

let part1 () =
    let elves = parse_elves ()


let part2 () =
    0