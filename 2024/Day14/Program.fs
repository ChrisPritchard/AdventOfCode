let input = System.IO.File.ReadAllLines "input.txt"

let width = 101
let height = 103

let mid_x = 50
let mid_y = 51

let vals =
    input
    |> Array.map (fun s ->
        let parts =
            s.Split("p=, v".ToCharArray(), System.StringSplitOptions.RemoveEmptyEntries)

        (int parts[0], int parts[1]), (int parts[2], int parts[3]))

let wrap n max =
    if n < 0 then max + n
    else if n >= max then n - max
    else n

let run_second vals =
    vals
    |> Array.map (fun ((x, y), (dx, dy)) ->
        let nx, ny = wrap (x + dx) width, wrap (y + dy) height
        (nx, ny), (dx, dy))

let result = List.fold (fun acc _ -> run_second acc) vals [ 1..100 ]

let quad_1 =
    result
    |> Array.filter (fun ((x, y), _) -> x < mid_x && y < mid_y)
    |> Array.length

let quad_2 =
    result
    |> Array.filter (fun ((x, y), _) -> x > mid_x && y < mid_y)
    |> Array.length

let quad_3 =
    result
    |> Array.filter (fun ((x, y), _) -> x < mid_x && y > mid_y)
    |> Array.length

let quad_4 =
    result
    |> Array.filter (fun ((x, y), _) -> x > mid_x && y > mid_y)
    |> Array.length

let sum = quad_1 * quad_2 * quad_3 * quad_4
printfn "Part 1: %d" sum

// let mutable tree = vals
// let mutable count = 0

// for _ in [ 0..10000 ] do
//     tree <- run_second tree
//     count <- count + 1

//     if (count - 153) % 101 = 0 then
//         printfn "%d" count

//         for j in 0 .. height - 1 do
//             for i in 0 .. width - 1 do
//                 let count = tree |> Array.filter (fun (p, _) -> p = (i, j)) |> Array.length

//                 if count > 0 then
//                     printf "%s" (count.ToString())
//                 else
//                     printf "."

//             printfn ""

// above when printed to a file will show the tree at the below count
printfn "Part 2: 6516"
