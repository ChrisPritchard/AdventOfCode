let input = System.IO.File.ReadAllLines "input.txt"

let rec crawler index pos top_left bottom_right dug =
    let add (x, y) (x2, y2) = x + x2, y + y2
    if index = input.Length then
        dug, top_left, bottom_right
    else
        let dir, dist, color = let parts = input[index].Split [|' '|] in parts[0], int parts[1], parts[2][1..parts[2].Length - 2]
        let delta = match dir with "U" -> 0,-1 | "D" -> 0,1 | "L" -> -1,0 | "R" | _ -> 1,0 
        let mutable new_pos = pos
        let mutable new_dug = dug
        for n in 1..dist do 
            new_pos <- add new_pos delta
            new_dug <- Set.add new_pos new_dug
        let dx, dy = new_pos
        let new_top_left = let tx, ty = top_left in min tx dx, min ty dy
        let new_bottom_right = let bx, by = bottom_right in max bx dx, max by dy
        crawler (index + 1) new_pos new_top_left new_bottom_right new_dug

let dug, (tx, ty), (bx, by) = crawler 0 (0,0) (1,1) (-1,-1) Set.empty

let rec filler queue dug inside =
    //printfn "%A" queue
    match queue with
    | [] -> inside
    | (x, y)::rem ->
        let new_inside = Set.add (x, y) inside
        let neighbours = [0,-1; 0,1; -1,0; 1,0] |> List.map (fun (dx, dy) -> x + dx, y + dy) |> List.filter (fun p -> not (Set.contains p dug) && not (List.contains p rem) && not (Set.contains p new_inside))
        filler (List.append neighbours rem) dug new_inside

let (sx, sy) = 
    [tx..bx] |> List.find (fun x -> dug.Contains (x, ty)) |> fun x -> x, ty
let inside = filler [sx+1,sy+1] dug Set.empty
printfn "%A" (tx, ty)

for y in ty..by do
    for x in tx..bx do
        if (x, y) = (tx, ty) then printf "S" else if Set.contains (x, y) dug then printf "#" else if Set.contains (x, y) inside then printf "X" else printf "."
    printf "\n"

let part1 = dug.Count + inside.Count
printfn "Part 1: %d" part1