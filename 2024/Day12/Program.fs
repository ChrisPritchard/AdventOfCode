let input = System.IO.File.ReadAllLines "input.txt"

// iterate through every cell, creating a struct with a x, y, type, adjacent count and group
// group is the group of any adjacent with the same type - if none are found then generate a new group
// once done, extract all groups and count the number of items in each group (which is the total area) and the 4 * area - total adjacent to get the wall count

let by_type =
    input
    |> Seq.indexed
    |> Seq.collect (fun (y, line) -> line |> Seq.indexed |> Seq.map (fun (x, c) -> (x, y), c))

let index = by_type |> Map.ofSeq
let for_processing = by_type |> Seq.toArray

let dirs = [| -1, 0; 0, -1; 1, 0; 0, 1 |]

let folder (acc, next_group) ((x, y), c) =
    let group_options =
        dirs
        |> Array.choose (fun (dx, dy) ->
            if not (Map.containsKey (x + dx, y + dy) index) || index[x + dx, y + dy] <> c then
                None
            else
                match Map.tryFind (x + dx, y + dy) acc with
                | Some(_, _, g) -> Some g
                | None -> Some -1)

    let adjacent = group_options.Length

    let group, consolidated =
        match Array.tryFind (fun g -> g <> -1) group_options with
        | Some g ->
            let other_groups = group_options |> Array.filter (fun o -> o <> g && o <> -1)

            let folder acc other =
                let folder2 acc2 k (c, adj, gr) =
                    if gr = other then Map.add k (c, adj, g) acc2 else acc2

                Map.fold folder2 acc acc

            let new_acc = Array.fold folder acc other_groups

            g, new_acc
        | None -> next_group, acc

    (Map.add (x, y) (c, adjacent, group) consolidated), if group = next_group then next_group + 1 else next_group

let (tagged, _) = Array.fold folder (Map.empty, 0) for_processing

let groups =
    tagged
    |> Map.toSeq
    |> Seq.map snd
    |> Seq.groupBy (fun (_, _, g) -> g)
    |> Seq.map (fun (_, group) -> group |> Seq.map (fun (_, adj, _) -> adj) |> Seq.toArray)
    |> Seq.toArray

let sum =
    groups
    |> Array.map (fun group -> group.Length, group.Length * 4 - Array.sum group)
    |> Array.sumBy (fun (area, fence) -> area * fence)

printfn "Part 1: %d" sum

// for part 2, number of sides = number of corners
// each cell has a numbers based on: two adjacent directions that are not the same type
// two adjacent directions that are, and a diagonal that isnt

let is_corner (x, y) c (dx1, dy1) (dx2, dy2) (diagx, diagy) =
    let d1 = x + dx1, y + dy1
    let d2 = x + dx2, y + dy2
    let diag = x + diagx, y + diagy

    let not_type d =
        not (Map.containsKey d index) || index[d] <> c

    (not_type d1 && not_type d2)
    || (not (not_type d1) && not (not_type d2) && not_type diag)

let to_check =
    [| dirs[0], dirs[1], (-1, -1)
       dirs[1], dirs[2], (1, -1)
       dirs[2], dirs[3], (1, 1)
       dirs[3], dirs[0], (-1, 1) |]

let tagged_with_corners =
    tagged
    |> Map.toSeq
    |> Seq.map (fun (p, (c, adj, g)) ->
        if adj = 0 then
            c, 4, g
        else
            let corner_count =
                to_check
                |> Array.filter (fun (d1, d2, diag) -> is_corner p c d1 d2 diag)
                |> Array.length

            c, corner_count, g)
    |> Array.ofSeq

let new_groups =
    tagged_with_corners
    |> Seq.groupBy (fun (_, _, g) -> g)
    |> Seq.map (fun (_, group) -> group |> Seq.map (fun (_, corners, _) -> corners) |> Seq.toArray)
    |> Seq.toArray

let new_sum =
    new_groups
    |> Array.map (fun group -> group.Length, Array.sum group)
    |> Array.sumBy (fun (area, sides) -> area * sides)

printfn "Part 2: %d" new_sum
