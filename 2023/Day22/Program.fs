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

let mutable remove_effect: Map<Brick[], int> = Map.empty
let rec find_all_supports bricks_to_remove =
    if remove_effect.ContainsKey bricks_to_remove then remove_effect[bricks_to_remove]
    else
        let supportless = with_supported_by |> Array.choose (fun (b, supported_by) -> if supported_by.Length > 0 && (Array.except bricks_to_remove supported_by).Length = 0 then Some b else None)
        if supportless.Length = 0 then 0 
        else 
            let total = supportless.Length + find_all_supports supportless
            remove_effect <- remove_effect.Add (bricks_to_remove, total)
            total

printfn "Part 2: %d" <| Array.sumBy find_all_supports (Array.map (Array.create 1) lowest_sorted)