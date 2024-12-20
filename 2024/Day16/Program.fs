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


// // use a DFS for part 2
// // search down until finding the target or exceeding cost/options
// // then walk back to last untested (node plus parent?) and test
// // when a tendril finds the target, walk back all its parents and add to the visited set

// let tendril_start = Set.empty.Add((start, '>'), (-1,-1))

// // we track the current path. each iteration we find its neighbours, and add them to the queue, with the path being the first
// // if there are no neighbours or a neighbour is the target, take the next item off the queue and walk back to that?
// // or track invalids/visited/exhausted? walk back until one isnt exhausted?

// track path. keep adding first neighbour not in path
// when reaching the target, walk back through path and add to all
// walk back to first path item that doesnt have a neighbour in path
