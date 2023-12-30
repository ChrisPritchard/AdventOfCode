let input = System.IO.File.ReadAllLines "input.txt"

let start = input |> Array.indexed |> Array.pick (fun (y, line) ->
    input[y].ToCharArray() |> Array.indexed |> Array.tryPick (fun (x, c) -> if c = 'S' then Some (x, y) else None))

let neighbours (x, y) = 
    [|-1,0; 1,0; 0,-1; 0,1|] 
    |> Array.map (fun (dx, dy) -> x + dx, y + dy) 
    |> Array.filter (fun (ox, oy) -> ox >= 0 && oy >= 0 && oy < input.Length && ox < input[oy].Length)
    |> Array.filter (fun (ox, oy) -> input[oy][ox] <> '#')

let rec stepper rem queue =
    if rem = 0 then Array.length queue
    else
        let new_queue = queue |> Array.collect neighbours |> Array.distinct
        stepper (rem - 1) new_queue

let part1 = stepper 64 [|start|]
printfn "Part 1: %d" part1