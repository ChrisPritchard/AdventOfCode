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

let adjacent (x, y) dir =
    match dir with
    | North -> [|add (x, y) (-1, -1);add (x, y) (0, -1);add (x, y) (1, -1)|]
    | South -> [|add (x, y) (-1, 1);add (x, y) (0, 1);add (x, y) (1, 1)|]
    | West -> [|add (x, y) (-1, -1);add (x, y) (-1, 0);add (x, y) (-1, 1)|]
    | East -> [|add (x, y) (1, -1);add (x, y) (1, 0);add (x, y) (1, 1)|]

let in_dir (x, y) dir =
    match dir with
    | North -> add (x, y) (0, -1)
    | South -> add (x, y) (0, 1)
    | West -> add (x, y) (-1, 0)
    | East -> add (x, y) (1, 0)

let next_state elf existing dirs =
    if dirs |> Array.collect (adjacent elf) |> Array.forall (fun p -> not (Set.contains p existing)) then
        elf
    else
        let next = dirs |> Array.tryFind (fun d -> adjacent elf d |> Array.forall (fun p -> not (Set.contains p existing)))
        match next with
        | None -> elf
        | Some(d) -> in_dir elf d

let parse_elves () =
    let input = readEmbedded "day23"
    input |> Array.indexed |> Array.fold (fun acc (y, line) ->
        line |> Seq.indexed |> Seq.fold (fun acc (x, char) -> 
            if char <> '#' then acc
            else Set.add (x, y) acc) acc) Set.empty

let part1 () =
    let elves = parse_elves ()

    let count_free_spaces elves = 
        let top = elves |> Array.minBy snd |> snd
        let left = elves |> Array.minBy fst |> fst
        let bottom = elves |> Array.maxBy snd |> snd
        let right = elves |> Array.maxBy fst |> fst
        let elves = Set.ofArray elves
        [|top..bottom|] |> Array.collect (fun y -> [|left..right|] |> Array.filter (fun x -> not (Set.contains (x, y) elves))) |> Array.length

    let rec processor elves turn =
        let dirs = sort_by turn
        let next_candidates = elves |> Set.toArray |> Array.map (fun e -> next_state e elves dirs, e) |> Array.groupBy fst
        let after_rules = next_candidates |> Array.collect (fun (n, matches) -> if matches.Length = 1 then [|n|] else matches |> Array.map snd)
        let next_turn = turn + 1
        if next_turn < 10 then
            processor (Set.ofArray after_rules) next_turn
        else
            count_free_spaces after_rules

    processor elves 0

let part2 () =

    let elves = parse_elves ()
    
    let rec processor elves turn =
        let dirs = sort_by turn
        let next_candidates = elves |> Set.toArray |> Array.map (fun e -> next_state e elves dirs, e) |> Array.groupBy fst
        let after_rules = next_candidates |> Array.collect (fun (n, matches) -> if matches.Length = 1 then [|n|] else matches |> Array.map snd) |> Set.ofArray

        let diff = Set.difference elves after_rules
        let next_turn = turn + 1
        if Set.isEmpty diff then next_turn
        else processor after_rules next_turn

    processor elves 0
