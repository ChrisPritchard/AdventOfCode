let input = System.IO.File.ReadAllLines "input.txt"

let connections = 
    input |> Array.collect (fun line -> 
        let parts = line.Split (": ".ToCharArray(), System.StringSplitOptions.RemoveEmptyEntries)
        Array.append [|parts[0], parts[1..]|] (parts[1..] |> Array.map (fun op -> op, [|parts[0]|])))
    |> Array.groupBy fst
    |> Array.map (fun (k, a) -> 
        let connected = a |> Array.collect (snd >> id) |> Array.distinct
        k, connected)

// solution based on https://github.com/WilliamVernaschi/aoc2023/blob/main/Cpp/day25.cpp

let E_map = Map.ofArray connections
let vertices = connections |> Array.map fst

let dfs E v =
    let mutable visited = Set.empty
    let rec dfs v = 
        visited <- visited.Add v
        for w in Map.find v E do
            if not (visited.Contains w) then
                dfs w
    dfs v
    visited

let mutable dist = Map.empty
let bfs start = 
    let q = System.Collections.Generic.PriorityQueue()
    q.Enqueue (start, 0)
    dist <- dist |> Map.add start (Map.empty.Add (start, 0))
    while q.Count > 0 do
        let u = q.Dequeue()
        for v in E_map[u] do
            if not (Map.containsKey v dist[start]) then
                let v_dist = dist[start][u] + 1
                dist <- dist |> Map.add start (dist[start].Add (v, v_dist))
                q.Enqueue (v, v_dist)
for v in vertices do
    bfs v

let rand = System.Random()
let can_be_bridge (u, v) =
    let mutable tries, failures = 300, 0
    for i in 0..tries-1 do
        let random_v = vertices[rand.Next (0,vertices.Length)]
        if abs (dist[u][random_v] - dist[v][random_v]) = 0 then
            failures <- failures + 1
    float failures <= 0.05 * float tries

let all_edges = connections |> Array.collect (fun (k, a) -> a |> Array.map (fun v -> if k > v then k, v else v, k)) |> Array.distinct
let remove_edge (a, b) (E: Map<string, string[]>) =
    Map.add a (Array.except [|b|] E[a]) E
    |> Map.add b (Array.except [|a|] E[b])

try
    for i in 0..all_edges.Length-1 do
        if can_be_bridge all_edges[i] then
            for j in i+1..all_edges.Length-1 do
                if can_be_bridge all_edges[j] then
                    for k in j+1..all_edges.Length-1 do
                        if can_be_bridge all_edges[k] then
                            let E = E_map |> remove_edge all_edges[i] |> remove_edge all_edges[j] |> remove_edge all_edges[k]
                            let res = dfs E (fst connections[0])
                            if res.Count < connections.Length then
                                failwithf "%d" (res.Count * (connections.Length - res.Count))
with
| e -> printfn "Part 1 (no part 2): %s" e.Message
