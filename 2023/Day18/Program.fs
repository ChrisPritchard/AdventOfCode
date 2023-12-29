let input = System.IO.File.ReadAllLines "input.txt"

// for part 2, part 1's 'crawler' solution will not work
// instead get points and calculate shoelace formula: https://en.wikipedia.org/wiki/Shoelace_formula
// then use area with picks theorem to get interior points: https://en.wikipedia.org/wiki/Pick%27s_theorem

let shoe_lace (points: (int64 * int64)[]) = 
    let mutable sum = points |> Array.windowed 2 |> Array.map (fun a -> a[0], a[1]) |> Array.sumBy (fun ((x1, y1), (x2, y2)) ->  (x1 * y2) - (x2 * y1))
    let (x1, y1), (x2, y2) = Array.last points, Array.head points
    sum <- sum + ((x1 * y2) - (x2 * y1))
    abs sum / 2L

let rev_picks boundary_points area = 
    area + 1L - (boundary_points / 2L)

let part1_points = 
    input 
    |> Array.map (fun line -> let parts = line.Split [|' '|] in parts[0], int64 parts[1])
    |> Array.mapFold (fun (x, y) (dir, dist) -> 
        let (dx, dy) = match dir with "U" -> 0L,-1L | "D" -> 0,1 | "L" -> -1,0 | "R" | _ -> 1,0 
        let next = x + dist * dx, y + dist * dy
        next, next) (0, 0)
    |> fst

let part1_boundary = input |> Array.sumBy (fun line -> let parts = line.Split [|' '|] in int64 parts[1])

let part1 = 
    let area = shoe_lace part1_points
    let interior = rev_picks part1_boundary area
    part1_boundary + interior

printfn "Part 1: %d" part1

let part2_points = 
    input 
    |> Array.map (fun line -> let parts = line.Split [|' '|] in parts[2])
    |> Array.mapFold (fun (x, y) code -> 
        let dir = code[7]
        let (dx, dy) = match dir with '0' -> 1L,0L | '1' -> 0,1 | '2' -> -1,0 | '3' | _ -> 0,-1
        let dist = System.Convert.ToInt64(code[2..6], 16)
        let next = int64 x + dist * dx, int64 y + dist * dy
        next, next) (0, 0)
    |> fst

let part2_boundary = input |> Array.sumBy (fun line -> 
    let parts = line.Split [|' '|] in System.Convert.ToInt64(parts[2][2..6], 16))

let part2 = 
    let area = shoe_lace part2_points
    let interior = rev_picks part2_boundary area
    part2_boundary + interior

printfn "Part 2: %d" part2