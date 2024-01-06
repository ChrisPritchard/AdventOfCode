let input = System.IO.File.ReadAllLines "input.txt"

let points = 
    input |> Array.indexed |> Array.collect (fun (y, line) -> 
        line.ToCharArray() |> Array.indexed |> Array.choose (fun (x, c) -> 
            if c = '#' then Some (x, y) else None))

let updown = [|0..input.Length - 1|]
let across = [|0..input[0].Length - 1|]
let empty_rows = 
    updown |> Array.filter (fun y -> across |> Array.forall (fun x -> not (Array.contains (x, y) points)))
let empty_cols = 
    across |> Array.filter (fun x -> updown |> Array.forall (fun y -> not (Array.contains (x, y) points)))

let expand_universe factor = 
    points |> Array.map (fun (x, y) ->
        let before_empty_rows = empty_rows |> Array.filter (fun cy -> cy < y) |> Array.length
        let before_empty_cols = empty_cols |> Array.filter (fun cx -> cx < x) |> Array.length
        int64 x + ((factor - 1L) * int64 before_empty_cols), int64 y + ((factor - 1L) * int64 before_empty_rows))

let dist_between (x1, y1) (x2, y2) =
    abs (x2 - x1) + abs (y2 - y1)

let shortest_dists points = 
    let mutable total_shortest_distances = 0L
    let mutable calculated = Set.empty
    for a in points do
        for b in points do
            if not (Set.contains (a, b) calculated) && not (Set.contains (b, a) calculated) then
                calculated <- Set.add (a, b) calculated
                total_shortest_distances <- total_shortest_distances + dist_between a b
    total_shortest_distances

let expanded_points = expand_universe 2
let part1_dists = shortest_dists expanded_points
printfn "Part 1: %d" part1_dists

let much_expanded_points = expand_universe 1000000
let part2_dists = shortest_dists much_expanded_points
printfn "Part 2: %d" part2_dists