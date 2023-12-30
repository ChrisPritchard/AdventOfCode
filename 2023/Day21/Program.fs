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

