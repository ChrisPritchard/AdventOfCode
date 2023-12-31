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

let all_valid_tiles = 
    [0..input.Length-1] |> List.collect (fun y -> 
        [0..input[y].Length-1] |> List.choose (fun x -> let point = (x, y) in if is_clear point then Some point else None))
        |> Array.ofList

let neighbours (x, y) = 
    [|-1,0; 1,0; 0,-1; 0,1|] 
    |> Array.map (fun (dx, dy) -> x + dx, y + dy) 
    |> Array.filter is_clear

let all_neighbours = all_valid_tiles |> Array.map (fun p -> p, neighbours p) |> Map.ofArray

let distances =
    let mutable result = Map.empty.Add (start, 0)
    let mutable queue = System.Collections.Generic.PriorityQueue()
    queue.Enqueue (start, 0)
    while queue.Count > 0 do
        let next = queue.Dequeue ()
        if Map.containsKey next all_neighbours then
            let neighbours = all_neighbours[next] |> Array.filter (fun p -> not (Map.containsKey p result))
            let dist = result[next] + 1
            for n in neighbours do
                result <- result.Add (n, dist)
                queue.Enqueue (n, dist)
    result

printfn "Part 1: %d" <| (Map.filter (fun _ d -> d <= 64 && d % 2 = 64 % 2) distances).Count

