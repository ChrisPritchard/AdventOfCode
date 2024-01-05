let input = System.IO.File.ReadAllLines "input.txt"

let points_and_speeds = input |> Array.map (fun line -> 
    line.Split (", @".ToCharArray(), System.StringSplitOptions.RemoveEmptyEntries)
    |> Array.map decimal |> fun a -> (a[0],a[1],a[2]), (a[3],a[4],a[5]))

let find_intersection (x1: decimal, y1: decimal, _) (dx1: decimal, dy1: decimal, _) (x2: decimal, y2: decimal, _) (dx2: decimal, dy2: decimal, _) = 
    let det = dx1 * dy2 - dy1 * dx2
    if det = 0m then None
    else
        let t = ((x2 - x1) * dy2 - (y2 - y1) * dx2) / det
        let s = ((x2 - x1) * dy1 - (y2 - y1) * dx1) / det
        if t >= 0m && s >= 0m then
            let intersection_x = x1 + t * dx1
            let intersection_y = y1 + t * dy1
            Some (intersection_x, intersection_y, t)
        else
            None
    
let area_min, area_max = 200000000000000.m, 400000000000000.m

let intersections = Array.fold (fun acc (p, v) -> 
    let intersections = 
        Array.filter (fun (op, ov) -> 
        if op = p || Set.contains (p, op) acc || Set.contains (op, p) acc then false
        else
            match find_intersection p v op ov with
            | None -> false
            | Some (x, y, t) -> t > 0m && x >= area_min && x <= area_max && y >= area_min && y <= area_max) points_and_speeds
    Array.fold (fun acc (op, _) -> Set.add (p, op) acc) acc intersections) Set.empty points_and_speeds

let part1 = Set.count intersections
printfn "Part 1: %d" part1

// solution for part 2 required linear algebra, which i am not good at (at all)
// replicated solution from https://github.com/tmbarker/advent-of-code/blob/main/Solutions/Y2023/D24/Solution.cs

let partial_pivot (a: decimal[,]) n =
    for i in 0..n-1 do
        let mutable pivot_row = i
        for j in i+1..n-1 do
            if abs a[j, i] > abs a[pivot_row, i] then
                pivot_row <- j
        if pivot_row <> i then
            for j in i..n do
                let old = a[i, j]
                a[i, j] <- a[pivot_row, j]
                a[pivot_row, j] <- old
        for j in i+1..n-1 do
            let factor = a[j, i] / a[i, i]
            for k in i..n do
                a[j, k] <- a[j, k] - factor * a[i, k]

let back_substitute (a: decimal[,]) n (x: decimal[]) =
    for i in n-1..(-1)..0 do
        let mutable sum = 0.m
        for j in i+1..n-1 do
            sum <- sum + a[i, j] * x[j]
        x[i] <- (a[i, n] - sum) / a[i, i]

let linear_solve (a: decimal[,]) =
    let n = a.GetLength 0
    let x = Array.create n 0.m
    partial_pivot a n
    back_substitute a n x
    x

let coefficients_1 a b =
    let (asx, asy, _), (avx, avy, _) = a
    let (bsx, bsy, _), (bvx, bvy, _) = b
    [|
        bvy - avy
        avx - bvx
        0.m
        asy - bsy
        bsx - asx
        0.m
        bsx * bvy - bsy * bvx - asx * avy + asy * avx 
    |]

let coefficients_2 a b =
    let (asx, asy, asz), (avx, avy, avz) = a
    let (bsx, bsy, bsz), (bvx, bvy, bvz) = b
    [|
        bvz - avz
        0.m
        avx - bvx
        asz - bsz
        0.m
        bsx - asx
        bsx * bvz - bsz * bvx - asx * avz + asz * avx
    |]

let coefficients_3 a b =
    let (_, asy, asz), (_, avy, avz) = a
    let (_, bsy, bsz), (_, bvy, bvz) = b
    [|
        0.m
        avz - bvz
        bvy - avy
        0.m
        bsz - asz
        asy - bsy
        -bsy * bvz + bsz * bvy + asy * avz - asz * avy
    |]

let fill_row (matrix: decimal[,]) row (vals: decimal[]) =
    for j in 0..vals.Length-1 do
        matrix[row, j] <- vals[j]

let matrix = Array2D.create 6 7 0.m
fill_row matrix 0 (coefficients_1 points_and_speeds[0] points_and_speeds[1])
fill_row matrix 1 (coefficients_1 points_and_speeds[0] points_and_speeds[2])
fill_row matrix 2 (coefficients_2 points_and_speeds[0] points_and_speeds[1])
fill_row matrix 3 (coefficients_2 points_and_speeds[0] points_and_speeds[2])
fill_row matrix 4 (coefficients_3 points_and_speeds[0] points_and_speeds[1])
fill_row matrix 5 (coefficients_3 points_and_speeds[0] points_and_speeds[2])

let part2 = linear_solve matrix |> Array.take 3 |> Array.sumBy (fun v -> System.Math.Round v)
printfn "Part 2: %M" part2