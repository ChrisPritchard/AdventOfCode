let input = System.IO.File.ReadAllLines "input.txt"

let start = input |> Array.indexed |> Array.pick (fun (y, line) ->
    input[y].ToCharArray() |> Array.indexed |> Array.tryPick (fun (x, c) -> if c = 'S' then Some (x, y) else None))

let add (x1, y1) (x2, y2) = x1 + x2, y1 + y2
let width = input[0].Length
let height = input.Length
let is_clear (x, y) = 
    let mutable x, y = x, y
    while x < 0 do x <- x + width
    while y < 0 do y <- y + height
    while x > (width - 1) do x <- x - width
    while y > (height - 1) do y <- y - height
    input[y][x] <> '#'

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
                if dist <= 327 then
                    if not (visited.ContainsKey n) && is_clear n then
                        queue.Enqueue ((dist + 1, n), dist + 1)
    visited

let count_at_step step = 
    uint64 (Map.filter (fun _ d -> d <= step && d % 2 = step % 2) distances).Count

printfn "Part 1: %d" <| count_at_step 64

// based on https://github.com/bsadia/aoc_goLang/blob/53ed198e644324d559a366a17c712e1b8c6bb4fe/day21/main.go by bsadia, and their explanation here:
// https://www.reddit.com/r/adventofcode/comments/18nevo3/comment/keh8k0z/?utm_source=share&utm_medium=web2x&context=3

let polynomial = [|
        count_at_step 65    // steps % width = target (~265k % width)
        count_at_step 196   // second time above happens, 131 (width) + 65
        count_at_step 327   // third time, 131*2 + 65
    |]

let a = (polynomial[2] + polynomial[0] - (2UL*polynomial[1])) / 2UL
let b = polynomial[1] - polynomial[0] - a 
let c = polynomial[0]
let n = 26501365UL / 131UL
let part2 = (a*n*n) + (b*n) + c

printfn "Part 2: %d" part2