let input = System.IO.File.ReadAllLines "input.txt"

type Coords = int * int

[<Struct>]
type QueueItem =
    { point: Coords
      direction: char
      cost: int }

let target: Coords = input[0].Length - 2, 1
let memo = System.Collections.Generic.Dictionary<Coords, int>()

let rec path_finder (queue: QueueItem list) =
    match queue with
    | [] -> ()
    | head :: remainder when memo.ContainsKey target && memo[target] < head.cost -> path_finder remainder
    | head :: remainder ->
        if memo.ContainsKey head.point && memo[head.point] < head.cost then
            path_finder remainder
        else if head.point = target then
            memo[head.point] <- head.cost
            path_finder remainder
        else
            memo[head.point] <- head.cost
            let neighbours = find_neighbours head
            let new_queue = List.append neighbours remainder
            path_finder new_queue

and find_neighbours node : QueueItem list =
    let (x, y) = node.point

    let candidates =
        match node.direction with
        | '>' ->
            [ (x + 1, y), node.direction, node.cost + 1
              (x, y - 1), '^', node.cost + 1001
              (x, y + 1), 'v', node.cost + 1001 ]
        | '<' ->
            [ (x - 1, y), node.direction, node.cost + 1
              (x, y - 1), '^', node.cost + 1001
              (x, y + 1), 'v', node.cost + 1001 ]
        | '^' ->
            [ (x, y - 1), node.direction, node.cost + 1
              (x - 1, y), '<', node.cost + 1001
              (x + 1, y), '>', node.cost + 1001 ]
        | 'v'
        | _ ->
            [ (x, y + 1), node.direction, node.cost + 1
              (x - 1, y), '<', node.cost + 1001
              (x + 1, y), '>', node.cost + 1001 ]

    candidates
    |> List.choose (fun ((nx, ny), direction, cost) ->
        if
            nx < 0
            || ny < 0
            || nx = input[0].Length
            || ny = input.Length
            || input[ny][nx] = '#'
        then
            None
        else
            Some(
                { point = (nx, ny)
                  direction = direction
                  cost = cost }
            ))

let start = 1, input.Length - 2

path_finder
    [ { point = start
        direction = '>'
        cost = 0 } ]

let best_cost = memo[target]
printfn "Part 1: %d" best_cost
