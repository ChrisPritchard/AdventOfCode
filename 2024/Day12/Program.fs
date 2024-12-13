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

let folder (acc, next_group) ((x, y), c) =
    let group_options =
        [| -1, 0; 0, -1; 1, 0; 0, 1 |]
        |> Array.choose (fun (dx, dy) ->
            if not (Map.containsKey (x + dx, y + dy) index) || index[x + dx, y + dy] <> c then
                None
            else
                match Map.tryFind (x + dx, y + dy) acc with
                | Some(_, g) -> Some g
                | None -> Some -1)

    let adjacent = group_options.Length

    let group, consolidated =
        match Array.tryFind (fun g -> g <> -1) group_options with
        | Some g ->
            let other_groups = group_options |> Array.filter (fun o -> o <> g && o <> -1)

            let folder acc other =
                let folder2 acc2 k (adj, gr) =
                    if gr = other then Map.add k (adj, g) acc2 else acc2

                Map.fold folder2 acc acc

            let new_acc = Array.fold folder acc other_groups

            g, new_acc
        | None -> next_group, acc

    (Map.add (x, y) (adjacent, group) consolidated), if group = next_group then next_group + 1 else next_group

let (tagged, _) = Array.fold folder (Map.empty, 0) for_processing

let groups =
    tagged
    |> Map.toSeq
    |> Seq.map snd
    |> Seq.groupBy snd
    |> Seq.map (fun (_, group) -> group |> Seq.map fst |> Seq.toArray)
    |> Seq.toArray

let sum =
    groups
    |> Array.map (fun group -> group.Length, group.Length * 4 - Array.sum group)
    |> Array.sumBy (fun (area, fence) -> area * fence)

printfn "Part 1: %d" sum
