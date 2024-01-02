let input = System.IO.File.ReadAllLines "input.txt"

type Brick = (int * int * int) * (int * int * int)

let brick_ends: Brick[] = input |> Array.map (fun line -> 
    let p = Array.map int (line.Split [|',';'~'|]) in (p[0],p[1],p[2]),(p[3],p[4],p[5]))

let lowest_sorted = brick_ends |> Array.sortBy (fun ((_,_,z1),(_,_,z2)) -> min z1 z2)

let brick_cubes (brick: Brick) =
    let (x1, y1, z1), (x2, y2, z2) = brick
    seq {
        for x in x1..(if x2 < x1 then -1 else 1)..x2 do
            for y in y1..(if y2 < y1 then -1 else 1)..y2 do
                for z in z1..(if z2 < z1 then -1 else 1)..z2 do
                    yield (x, y, z)
    } |> Seq.toArray

let with_cubes = lowest_sorted |> Array.map (fun b -> b, brick_cubes b)

let drop (brick, cubes) (others: (Brick * (int * int * int)[])[]) = 
    let all_other_cubes = others |> Array.collect snd
    let drop_amount = Array.min (cubes |> Array.map (fun (x, y, z) -> 
        let highest = all_other_cubes |> Array.choose (fun (ox, oy, oz) -> if x = ox && y = oy then Some oz else None) |> fun a -> if Array.isEmpty a then -1 else Array.max a
        z - (highest + 1)))
    brick, cubes |> Array.map (fun (x, y, z) -> x, y, z - drop_amount)

let all_dropped = Array.fold (fun others brick_with_cubes -> Array.append others [|drop brick_with_cubes others|]) Array.empty with_cubes

let with_supported_by = all_dropped |> Array.map (fun (b, cubes) -> 
    b, all_dropped 
            |> Array.filter (fun (ob, ocubes) -> 
                ob <> b && (ocubes |> Array.exists (fun (ox, oy, oz) -> cubes |> Array.exists (fun (x, y, z) -> x = ox && y = oy && z = oz + 1))))
            |> Array.map (fun (b, _) -> b))

let doesnt_support_or_partially_supports = with_supported_by |> Array.filter (fun (b, _) -> 
    let supports = with_supported_by |> Array.filter (fun (_, supported_by) -> Array.contains b supported_by)
    supports.Length = 0 || (supports |> Array.forall (fun (_, supported_by) -> supported_by.Length > 1)))

printfn "Part 1: %d" doesnt_support_or_partially_supports.Length

// this takes a few seconds to run

let mutable removal_effects = Map.empty // all bricks removed if a given brick is removed
for brick in Array.rev lowest_sorted do
    let mutable to_remove = [|brick|]
    let mutable total_removed = Array.empty
    let mutable stack = with_supported_by
    while to_remove.Length > 0 do
        let mutable new_removes = Array.empty
        stack <- stack |> Array.choose (fun (b, supported_by) ->
            if Array.contains b to_remove || supported_by.Length = 0 then None
            else
                let new_supported_by = Array.except to_remove supported_by
                if new_supported_by.Length = 0 then
                    let affected = removal_effects.TryFind b |> Option.defaultValue Array.empty
                    let affected_with_brick = Array.append [|b|] affected
                    new_removes <- Array.append new_removes affected_with_brick
                    None
                else
                    Some (b, new_supported_by))
        total_removed <- Array.append total_removed new_removes
        to_remove <- new_removes
    removal_effects <- removal_effects.Add (brick, total_removed)

printfn "Part 2: %d" <| (Map.values removal_effects |> Seq.sumBy (Seq.length))