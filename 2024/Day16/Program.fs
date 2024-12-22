let input = System.IO.File.ReadAllLines "input.txt"

type QueueItem =
    { point: int * int
      dir: char
      cost: int }

let vector qi = qi.point, qi.dir

type MemoItem =
    { best_cost: int
      parents: ((int * int) * char) list }

let graph =
    input
    |> Seq.indexed
    |> Seq.collect (fun (y, line) ->
        line
        |> Seq.indexed
        |> Seq.filter (fun (_, c) -> c <> '#')
        |> Seq.map (fun (x, _) ->
            let point = (x, y)

            let neighbours =
                seq {
                    if input[y][x - 1] <> '#' then
                        yield '<', (x - 1, y)

                    if input[y][x + 1] <> '#' then
                        yield '>', (x + 1, y)

                    if input[y - 1][x] <> '#' then
                        yield '^', (x, y - 1)

                    if input[y + 1][x] <> '#' then
                        yield 'v', (x, y + 1)
                }

            point, Array.ofSeq neighbours))
    |> Map.ofSeq

let neighbours (node: QueueItem) =
    let neighbours = graph[node.point]

    let invalid =
        match node.dir with
        | '>' -> '<'
        | '<' -> '>'
        | '^' -> 'v'
        | _ -> '^'

    neighbours
    |> Array.filter (fun (d, _) -> d <> invalid)
    |> Array.map (fun (d, p) ->
        { point = p
          dir = d
          cost = if d = node.dir then node.cost + 1 else node.cost + 1001 })

let start = (1, input.Length - 2)
let target = (input[0].Length - 2, 1)

let tracking = System.Collections.Generic.Dictionary<(int * int) * char, MemoItem>()
tracking.Add((start, '>'), { best_cost = 0; parents = [] })

let get_memo vector =
    if tracking.ContainsKey vector then
        tracking[vector]
    else
        { best_cost = System.Int32.MaxValue
          parents = [] }

let mutable target_cost = System.Int32.MaxValue

let rec path_finder queue =
    match queue with
    | [] -> ()
    | n :: remainder when target_cost < n.cost || get_memo(vector n).best_cost < n.cost -> path_finder remainder
    | n :: remainder ->
        let options = neighbours n
        let mutable new_queue = remainder

        for option in options do
            let existing = get_memo (vector option)

            if existing.best_cost > option.cost then
                tracking[vector option] <-
                    { best_cost = option.cost
                      parents = [ vector n ] }
            else if existing.best_cost = option.cost then
                tracking[vector option] <-
                    { best_cost = option.cost
                      parents = List.distinct (vector n :: get_memo(vector option).parents) }

            if existing.best_cost >= option.cost then
                if option.point <> target then
                    new_queue <- option :: new_queue
                else
                    target_cost <- min target_cost option.cost

        path_finder new_queue

path_finder [ { point = start; dir = '>'; cost = 0 } ]

printfn "Part 1: %d" target_cost

let all_paths = System.Collections.Generic.HashSet<(int * int) * char>()

let target_reached =
    tracking
    |> Seq.filter (fun kv -> fst kv.Key = target)
    |> Seq.groupBy (fun kv -> kv.Value.best_cost)
    |> Seq.sortBy fst
    |> Seq.head
    |> snd
    |> Seq.map (fun kv -> kv.Key)

let rec reconstructor node =
    let parents = List.distinct tracking[node].parents

    for p in parents do
        if not (all_paths.Contains p) then
            ignore (all_paths.Add p)
            reconstructor p

for t in target_reached do
    ignore (all_paths.Add t)
    reconstructor t

let visited = Seq.map fst all_paths |> Set.ofSeq

printfn "Part 2: %d" (visited.Count)

for (y, line) in input |> Seq.indexed do
    for (x, c) in line |> Seq.indexed do
        let p =
            if c = '#' then '#'
            else if visited.Contains(x, y) then 'O'
            else '.'

        printf "%c" p

    printfn ""

// for (y, line) in input |> Seq.indexed do
//     for (x, c) in line |> Seq.indexed do
//         let p =
//             if c = '#' then
//                 "#\t"
//             else if visited.Contains(x, y) then
//                 tracking[x, y].best_cost.ToString() + "\t"
//             else
//                 "\t"

//         printf "%s" p

//     printfn ""
