let input = System.IO.File.ReadAllLines "input.txt"

let rec path_finder queue memo =
    match queue with
    | [] -> memo
    | (n, dir, cost, parent) :: remainder ->
        if Map.containsKey n memo && (let (o, _) = memo[n] in o < cost) then
            path_finder remainder memo
        else
            let new_memo = Map.add n (cost, parent) memo
            let neighbours = find_neighbours n dir cost
            let new_queue = List.append neighbours remainder
            path_finder new_queue new_memo

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
            Some((nx, ny), dir, cost, (x, y)))

let start = 1, input.Length - 2

let all_costs = path_finder [ start, '>', 0, (-1, -1) ] Map.empty

let target = input[0].Length - 2, 1

printfn "%A" all_costs[target]
