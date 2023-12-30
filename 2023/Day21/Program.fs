let input = System.IO.File.ReadAllLines "input.txt"

let start = input |> Array.indexed |> Array.pick (fun (y, line) ->
    input[y].ToCharArray() |> Array.indexed |> Array.tryPick (fun (x, c) -> if c = 'S' then Some (x, y) else None))

let is_clear (x, y) = 
    let dy = y % input.Length
    let dy = if dy < 0 then input.Length + dy else dy
    let dx = x % input[dy].Length
    let dx = if dx < 0 then input[dy].Length + dx else dx
    if dx < 0 then 
        failwithf "something wrong %d %d v %d %d" x y input.Length input[dy].Length
    input[dy][dx] <> '#'

let neighbours (x, y) = 
    [|-1,0; 1,0; 0,-1; 0,1|] 
    |> Array.map (fun (dx, dy) -> x + dx, y + dy) 
    |> Array.filter is_clear

let rec stepper rem queue =
    if rem = 0 then Array.length queue
    else
        let new_queue = queue |> Array.collect neighbours |> Array.distinct
        printfn "diff %d" (new_queue.Length - queue.Length)
        stepper (rem - 1) new_queue

let part1 = stepper 64 [|start|]
printfn "Part 1: %d" part1

// when a cell is visited, it will be visited ever second step after that
// this means we need not re-evaluate it or its neibours, just track when it was first visited
// for ever expanding neighbours, its possible the same map tile (but with different coords) will be visited for the 'first' time more than once
// could track each of these subsequent first visits by coord, against the original tile
// after the large step count (which is possibly small enough it could be iterated?) simply go through each tile, sum the mod2s of each visit, and thats the total
