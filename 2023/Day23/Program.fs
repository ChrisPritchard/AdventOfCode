let input = System.IO.File.ReadAllLines "input.txt"

let start = (1, 0)
let target = (input[0].Length - 2, input.Length - 1)

let find_edges (x, y) = [|-1,0; 1,0; 0,-1; 0,1|] |> Array.choose (fun (dx, dy) -> 
    let ox, oy = x + dx, y + dy
    if ox < 0 || oy < 0 || oy >= input.Length || ox >= input[oy].Length || input[oy][ox] = '#' then None 
    else Some (ox, oy))
let all_edges = [0..input.Length-1] |> List.collect (fun y -> [0..input[y].Length-1] |> List.choose (fun x -> 
    if input[y][x] = '#' then None else Some ((x, y), find_edges (x, y)))) |> Map.ofList

let vertices = all_edges |> Map.filter (fun p n -> p = start || p = target || Array.length n > 2) |> Map.keys |> Array.ofSeq
let expand_edges vertex acc = 
    let edges = all_edges[vertex]
    let rec follow_edge visited current length =
        let next = all_edges[current] |> Array.except visited |> Array.head
        if Array.contains next vertices then
            next, length + 1
        else
            follow_edge (Set.add next visited) next (length + 1)
    let full_edges = edges |> Array.map (fun edge -> follow_edge (Set.empty.Add vertex) edge 1)
    let mutable acc = acc
    for (end_vertex, length) in full_edges do
        acc <- Map.add (vertex)
    
let edges = vertices |> Array.fold (fun acc vertex -> if Map.containsKey vertex acc then acc else expand_edges vertex acc) Map.empty
printfn "%A" vertices

// vertex can be int * int
// edge can be int * int, start, length, end, int * int

// let rec bfs edges ongoing longest = 
//     let new_paths = ongoing |> Array.collect (fun (last, acc) -> 
//         let neighbours = edges last
//         neighbours |> Array.choose (fun n -> if Set.contains n acc then None else Some (n, Set.add n acc)))
//     let finished = Array.filter (fun (last, _) -> last = target) new_paths
//     // for (_, visited) in finished do
//     //     for y in 0..input.Length-1 do
//     //         for x in 0..input[y].Length-1 do
//     //             if visited.Contains (x, y) then printf "O" else printf "%c" (input[y][x])
//     //         printf "\n"
//     //     printfn ""
//     let longest = if finished.Length > 0 then max longest (Array.max (Array.map (snd >> Set.count) finished)) else longest
//     let ongoing = Array.except finished new_paths
//     if ongoing.Length > 0 then
//         bfs edges ongoing longest
//     else
//         longest - 1 // skip start

// let edges1 (x, y) =
//     let current = input[y][x]
//     if current = '>' then [|x + 1, y|]
//     else if current = 'v' then [|x, y + 1|]
//     else
//         [|-1,0; 1,0; 0,-1; 0,1|] |> Array.choose (fun (dx, dy) -> 
//             let ox, oy = x + dx, y + dy
//             if ox < 0 || oy < 0 || oy >= input.Length || ox >= input[oy].Length then None
//             else
//                 let c = input[oy][ox]
//                 if c = '.' || (c = '>' && dx = 1) || (c = 'v' && dy = 1) then Some (ox, oy)
//                 else None)

// //let part1 = bfs edges1 [|start, Set.empty.Add start|] 0
// //printfn "Part 1: %d" part1

// let edges2 (x, y) =
//     [|-1,0; 1,0; 0,-1; 0,1|] |> Array.choose (fun (dx, dy) -> 
//         let ox, oy = x + dx, y + dy
//         if ox < 0 || oy < 0 || oy >= input.Length || ox >= input[oy].Length then None
//         else
//             let c = input[oy][ox]
//             if c <> '#' then Some (ox, oy)
//             else None)

// let all_edges = [0..input.Length-1] |> List.collect (fun y -> [0..input[y].Length-1] |> List.choose (fun x -> if input[y][x] = '#' then None else Some ((x, y), edges2 (x, y)))) |> Map.ofList

// let rec dfs stack max_so_far =
//     match stack with
//     | [] -> max_so_far
//     | (next, visited, steps)::rem ->
//         if next = target then
//             let new_max = max max_so_far steps
//             if new_max <> max_so_far then
//                 printfn "%d" new_max
//             dfs rem new_max
//         else
//             let neighbours = all_edges[next] |> Array.filter (fun n -> not (Set.contains n visited))
//             if neighbours.Length = 0 then
//                 //let new_rem = match rem with [] -> [] | (last, _, steps)::rem -> (last, visited ,steps)::rem
//                 dfs rem max_so_far
//             else
//                 let new_head = neighbours |> Array.map (fun n -> n, Set.add n visited, steps + 1) |> List.ofArray
//                 dfs (List.append new_head rem) max_so_far

// printfn "Part 2: %d" <| dfs [start, Set.empty.Add start, 0] 0