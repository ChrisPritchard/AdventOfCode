let input = System.IO.File.ReadAllLines "input.txt"

let dimx, dimy = 70, 70

let coords =
    input
    |> Array.map (fun line ->
        line.Split ','
        |> Array.map (fun n -> System.Int32.Parse n)
        |> fun a -> a[0], a[1])

// for y in 0..dimy do
//     for x in 0..dimx do
//         if coords.Contains(x, y) then printf "#" else printf "."

//     printfn ""

let tracking = System.Collections.Generic.Dictionary<int * int, int>()
tracking.Add((0, 0), 0)

let rec pathfinder queue (rain_drops: Set<int * int>) =
    match queue with
    | [] ->
        if tracking.ContainsKey(dimx, dimy) then
            Some tracking[dimx, dimy]
        else
            None
    | (x, y) :: remainder ->
        let cost = tracking[x, y]

        let neighbours =
            [| -1, 0; 0, -1; 1, 0; 0, 1 |]
            |> Array.choose (fun (dx, dy) ->
                let nx, ny = dx + x, dy + y

                if nx < 0 || ny < 0 || nx > dimx || ny > dimy || rain_drops.Contains(nx, ny) then
                    None
                else
                    Some(nx, ny))

        let mutable new_queue = remainder

        for n in neighbours do
            if not (tracking.ContainsKey n) || tracking[n] > cost + 1 then
                tracking[n] <- cost + 1
                new_queue <- n :: new_queue

        pathfinder new_queue rain_drops

let part1_count = 1024
let target_cost = pathfinder [ 0, 0 ] (Array.take part1_count coords |> Set.ofArray)
printfn "Part 1: %d" target_cost.Value

let rec bin_search min max looking_for_blocked =
    let n = (max - min) / 2 + min
    tracking.Clear()
    tracking.Add((0, 0), 0)
    let target_cost = pathfinder [ 0, 0 ] (Array.take n coords |> Set.ofArray)

    if target_cost.IsSome then
        if max - min = 1 then
            n + 1
        else if looking_for_blocked then
            bin_search n max looking_for_blocked
        else
            bin_search n max false
    else if max - min = 1 then
        n
    else if not looking_for_blocked then
        bin_search min n looking_for_blocked
    else
        bin_search min n true

let block_count = bin_search part1_count input.Length true
let x, y = coords[block_count - 1]
printfn "Part 2: %d,%d" x y
