let input = System.IO.File.ReadAllLines "input.txt"

let start = input |> Array.indexed |> Array.pick (fun (y, line) ->
    input[y].ToCharArray() |> Array.indexed |> Array.tryPick (fun (x, c) -> if c = 'S' then Some (x, y) else None))

let add (x1, y1) (x2, y2) = x1 + x2, y1 + y2
let is_valid (x, y) = x >= 0 && y >= 0 && y < input.Length && x < input[y].Length
let is_clear (x, y) = input[y][x] <> '#'

let distances =
    let mutable visited = Map.empty
    let mutable queue = System.Collections.Generic.PriorityQueue()
    queue.Enqueue ((0, start), 0)
    while queue.Count > 0 do
        let (dist, next) = queue.Dequeue ()
        if not (visited.ContainsKey next) then
            visited <- visited.Add (next, dist)
            for delta in [|-1,0; 1,0; 0,-1; 0,1|] do
                let n = add next delta
                if is_valid n then
                    if not (visited.ContainsKey n) && is_clear n then
                        queue.Enqueue ((dist + 1, n), dist + 1)
    visited

printfn "Part 1: %d" <| (Map.filter (fun _ d -> d <= 64 && d % 2 = 0) distances).Count

// part 2 is based on the excellent https://github.com/villuna/aoc23/wiki/A-Geometric-solution-to-advent-of-code-2023,-day-21
// which also verified my approach to part 1 (revised from the original stepper)

let even_corners = uint64 (Map.filter (fun  _ d -> d > 65 && d % 2 = 0) distances).Count
let odd_corners = uint64 (Map.filter (fun  _ d -> d > 65 && d % 2 = 1) distances).Count

let even_full = uint64 (Map.filter (fun _ d -> d % 2 = 0) distances).Count
let odd_full = uint64 (Map.filter (fun _ d -> d % 2 = 1) distances).Count

let n = uint64 ((26501365 - (input[0].Length / 2)) / input[0].Length)

let even = n * n
let odd = (n + 1UL) * (n + 1UL)

let part2 = odd * odd_full + even * even_full - (n + 1UL) * odd_corners + n * even_corners

printfn "Part 2: %d" part2