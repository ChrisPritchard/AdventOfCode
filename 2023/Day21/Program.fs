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
        stepper (rem - 1) new_queue

let part1 = stepper 64 [|start|]
printfn "Part 1: %d" part1

// when a cell is visited, it will be visited ever second step after that
// this means we need not re-evaluate it or its neibours, just track when it was first visited
// for ever expanding neighbours, its possible the same map tile (but with different coords) will be visited for the 'first' time more than once
// could track each of these subsequent first visits by coord, against the original tile
// after the large step count (which is possibly small enough it could be iterated?) simply go through each tile, sum the mod2s of each visit, and thats the total

// can precalculate all neighbours

let all_valid_tiles = 
    [0..input.Length-1] |> List.collect (fun y -> 
        [0..input[y].Length-1] |> List.choose (fun x -> let point = (x, y) in if is_clear point then Some point else None))
        |> Array.ofList

let all_neighbours = all_valid_tiles |> Array.map (fun p -> p, neighbours p) |> Map.ofArray

// tiles tracked by their real position and relative position. the first used for expansion, the second for visited
// every tile is reachable via bfs, the path from start to the tile is the steps taken. maybe a full djikstra map?

let distances =
    let mutable result = Map.empty.Add (start, 0)
    let mutable queue = [start]
    while not queue.IsEmpty do
        let next = List.head queue
        if Map.containsKey next all_neighbours then
            let neighbours = all_neighbours[next] |> Array.filter (fun p -> not (Map.containsKey p result))
            let dist = result[next] + 1
            for n in neighbours do
                result <- result.Add (n, dist)
            queue <- List.append (List.ofArray neighbours) (List.tail queue)
        else
            queue <- List.tail queue |> List.sortBy (fun p -> result[p])
    result

// to get count at step, all tiles at step and all tiles mod 2 below it
let step_count_at_64 = distances |> Map.filter (fun _ d -> d <= 64 && d % 2 = 64 % 2) |> Map.count
printfn "%d" step_count_at_64

// for y in [0..input.Length-1] do
//     for x in [0..input[y].Length-1] do
//         if distances.ContainsKey (x, y) then printf "\t%d" distances[x, y]
//         else printf "\t%c" (input[y][x])
//     printf "\n"