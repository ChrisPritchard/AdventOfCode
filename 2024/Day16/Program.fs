let input = System.IO.File.ReadAllLines "input.txt"

let target = input[0].Length - 2, 1
let memo = System.Collections.Generic.Dictionary<int * int, int>()

let rec path_finder queue =
    match queue with
    | [] -> ()
    | (_, _, cost) :: remainder when memo.ContainsKey target && memo[target] < cost -> path_finder remainder
    | (n, dir, cost) :: remainder ->
        if memo.ContainsKey(n) && memo[n] < cost then
            path_finder remainder
        else if n = target then
            memo[n] <- cost
            path_finder remainder
        else
            memo[n] <- cost
            let neighbours = find_neighbours n dir cost
            let new_queue = List.append neighbours remainder
            path_finder new_queue

and find_neighbours (x, y) current_dir current_cost =
    let candidates =
        match current_dir with
        | '>' ->
            [ (x + 1, y), current_dir, current_cost + 1
              (x, y - 1), '^', current_cost + 1001
              (x, y + 1), 'v', current_cost + 1001 ]
        | '<' ->
            [ (x - 1, y), current_dir, current_cost + 1
              (x, y - 1), '^', current_cost + 1001
              (x, y + 1), 'v', current_cost + 1001 ]
        | '^' ->
            [ (x, y - 1), current_dir, current_cost + 1
              (x - 1, y), '<', current_cost + 1001
              (x + 1, y), '>', current_cost + 1001 ]
        | 'v'
        | _ ->
            [ (x, y + 1), current_dir, current_cost + 1
              (x - 1, y), '<', current_cost + 1001
              (x + 1, y), '>', current_cost + 1001 ]

    candidates
    |> List.choose (fun ((nx, ny), dir, cost) ->
        if
            nx < 0
            || ny < 0
            || nx = input[0].Length
            || ny = input.Length
            || input[ny][nx] = '#'
        then
            None
        else
            Some((nx, ny), dir, cost))

let start = 1, input.Length - 2

path_finder [ start, '>', 0 ]

let best_cost = memo[target]
printfn "Part 1: %d" best_cost

// calculate all paths
