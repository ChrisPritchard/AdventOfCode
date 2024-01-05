let input = System.IO.File.ReadAllLines "input.txt"

let points_and_speeds = input |> Array.map (fun line -> 
    line.Split (", @".ToCharArray(), System.StringSplitOptions.RemoveEmptyEntries)
    |> Array.map float |> fun a -> (a[0],a[1],a[2]), (a[3],a[4],a[5]))

let find_intersection (x1: float, y1: float, _) (dx1: float, dy1: float, _) (x2: float, y2: float, _) (dx2: float, dy2: float, _) = 
    let det = dx1 * dy2 - dy1 * dx2
    if det = 0 then None
    else
        let t = ((x2 - x1) * dy2 - (y2 - y1) * dx2) / det
        let s = ((x2 - x1) * dy1 - (y2 - y1) * dx1) / det
        if t >= 0 && s >= 0 then
            let intersection_x = x1 + t * dx1
            let intersection_y = y1 + t * dy1
            Some (intersection_x, intersection_y, t)
        else
            None
    
let area_min, area_max = 200000000000000., 400000000000000.

let intersections = Array.fold (fun acc (p, v) -> 
    let intersections = 
        Array.filter (fun (op, ov) -> 
        if op = p || Set.contains (p, op) acc || Set.contains (op, p) acc then false
        else
            match find_intersection p v op ov with
            | None -> false
            | Some (x, y, t) -> t > 0 && x >= area_min && x <= area_max && y >= area_min && y <= area_max) points_and_speeds
    Array.fold (fun acc (op, _) -> Set.add (p, op) acc) acc intersections) Set.empty points_and_speeds

let part1 = Set.count intersections
printfn "Part 1: %d" part1
