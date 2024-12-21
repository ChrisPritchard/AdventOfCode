let input = System.IO.File.ReadAllLines "input.txt"

type Vector = int * int * char


[<Struct>]
type QueueItem =
    { vector: Vector
      cost: int
      parent: Vector }

[<Struct>]
type PathItem = { cost: int; parents: Vector list }

let is_target (v: Vector) =
    let (x, y, _) = v in x = input[0].Length - 2 && y = 1

let memo = System.Collections.Generic.Dictionary<Vector, PathItem>()

let mutable best_cost = System.Int32.MaxValue

let rec path_finder (queue: QueueItem list) =
    match queue with
    | [] -> ()
    | head :: remainder when best_cost < head.cost -> path_finder remainder
    | head :: remainder ->
        if memo.ContainsKey head.vector && memo[head.vector].cost < head.cost then
            path_finder remainder
        else
            if memo.ContainsKey head.vector then
                if memo[head.vector].cost > head.cost then
                    memo[head.vector] <-
                        { cost = head.cost
                          parents = [ head.parent ] }
                else
                    memo[head.vector] <-
                        { cost = head.cost
                          parents = head.parent :: memo[head.vector].parents }
            else
                memo[head.vector] <-
                    { cost = head.cost
                      parents = [ head.parent ] }

            if is_target head.vector then
                best_cost <- min best_cost head.cost
                path_finder remainder
            else
                let neighbours = find_neighbours head
                let new_queue = List.append neighbours remainder
                path_finder new_queue

and find_neighbours node : QueueItem list =

    let candidates =
        match node.vector with
        | x, y, '>' ->
            [ (x + 1, y, '>'), node.cost + 1
              (x, y - 1, '^'), node.cost + 1001
              (x, y + 1, 'v'), node.cost + 1001 ]
        | x, y, '<' ->
            [ (x - 1, y, '<'), node.cost + 1
              (x, y - 1, '^'), node.cost + 1001
              (x, y + 1, 'v'), node.cost + 1001 ]
        | x, y, '^' ->
            [ (x, y - 1, '^'), node.cost + 1
              (x - 1, y, '<'), node.cost + 1001
              (x + 1, y, '>'), node.cost + 1001 ]
        | x, y, 'v'
        | x, y, _ ->
            [ (x, y + 1, 'v'), node.cost + 1
              (x - 1, y, '<'), node.cost + 1001
              (x + 1, y, '>'), node.cost + 1001 ]

    candidates
    |> List.choose (fun ((nx, ny, direction), cost) ->
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
                { vector = (nx, ny, direction)
                  cost = cost
                  parent = node.vector }
            ))

let start = 1, input.Length - 2, '>'

path_finder
    [ { vector = start
        cost = 0
        parent = -1, -1, ' ' } ]

printfn "Part 1: %d" best_cost
