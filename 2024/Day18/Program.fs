let input = System.IO.File.ReadAllLines "input.txt"

let dimx, dimy = 70, 70
let part1_count = 1024

let coords =
    input
    |> Array.take part1_count
    |> Array.map (fun line ->
        line.Split ','
        |> Array.map (fun n -> System.Int32.Parse n)
        |> fun a -> a[0], a[1])
    |> Set.ofArray

for y in 0..dimy do
    for x in 0..dimx do
        if coords.Contains(x, y) then printf "#" else printf "."

    printfn ""

let tracking = System.Collections.Generic.Dictionary<int * int, int>()
tracking.Add((0, 0), 0)

let rec pathfinder queue =
    match queue with
    | [] -> tracking[dimx, dimy]
    | (x, y) :: remainder ->
        let cost = tracking[x, y]

        let neighbours =
            [| -1, 0; 0, -1; 1, 0; 0, 1 |]
            |> Array.choose (fun (dx, dy) ->
                let nx, ny = dx + x, dy + y

                if nx < 0 || ny < 0 || nx > dimx || ny > dimy || coords.Contains(nx, ny) then
                    None
                else
                    Some(nx, ny))

        let mutable new_queue = remainder

        for n in neighbours do
            if not (tracking.ContainsKey n) || tracking[n] > cost + 1 then
                tracking[n] <- cost + 1
                new_queue <- n :: new_queue

        pathfinder new_queue

let target_cost = pathfinder [ 0, 0 ]
printfn "Part 1: %d" target_cost
