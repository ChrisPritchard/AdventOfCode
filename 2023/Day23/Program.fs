let input = System.IO.File.ReadAllLines "input.txt"

let start = (1, 0)
let target = (input[0].Length - 2, input.Length - 1)

let point_to_neighbours =
    [|0..input.Length - 1|] |> Array.collect (fun y ->
        [|0..input[y].Length - 1|] |> Array.choose (fun x -> 
            if input[y][x] = '#' then None
            else
                let local_neighbours = [|-1,0; 1,0; 0,-1; 0,1|] |> Array.choose (fun (dx, dy) -> 
                    let ox, oy = x + dx, y + dy
                    if ox < 0 || oy < 0 || oy >= input.Length || ox >= input[oy].Length then None 
                    else
                        let c = input[oy][ox]
                        if c = '#' then None else Some (c, (ox, oy)))
                Some ((x, y), local_neighbours)
            ))
    |> Map.ofArray

let vertices = point_to_neighbours |> Map.filter (fun _ n -> n.Length <> 2)

let ungrouped_edges = point_to_neighbours |> Map.filter (fun _ n -> n.Length = 2)
let mutable edges, visited = Array.empty, Set.empty
for (ingress, edge) in vertices.Values |> Seq.collect id do
    if not (visited.Contains edge) then
        visited <- visited.Add edge
        let start = ungrouped_edges[edge] |> Array.find (fun (_, n) -> vertices.ContainsKey n) |> snd
        let cliff_start = (ingress = '>' && fst start > fst edge) || (ingress = 'v' && snd start > snd edge)
        let mutable current, egress, length, finished = edge, ingress, 1, false
        while not finished do
            let next = 
                ungrouped_edges[current] 
                |> Array.filter (fun (_, e) -> Map.containsKey e ungrouped_edges && not (Set.contains e visited))
            if next.Length = 0 then
                finished <- true
            else
                length <- length + 1
                current <- snd next[0]
                egress <- fst next[0]
                visited <- visited.Add current
        let finish = ungrouped_edges[current] |> Array.find (fun (_, n) -> vertices.ContainsKey n) |> snd
        let cliff_end = (egress = '>' && fst finish > fst current) || (egress = 'v' && snd finish > snd current)
        edges <- Array.append edges [|(start, finish <> target && not cliff_end || cliff_start, finish, not cliff_start || cliff_end, length)|]

let vertices_with_edges = 
    let starting = Array.groupBy (fun (start, _, _, _, _) -> start) edges
                |> Array.map (fun (vertex, edges) -> vertex, edges |> Array.map (fun (_, cliff, finish, _, length) -> (finish, cliff, length)))
    let ending = Array.groupBy (fun (_, _, finish, _, _) -> finish) edges
                |> Array.map (fun (vertex, edges) -> vertex, edges |> Array.map (fun (start, _, _, cliff, length) -> (start, cliff, length)))
    let ending_map = Map.ofArray ending
    starting |> Array.map (fun (s, a) -> if ending_map.ContainsKey s then s, Array.append a ending_map[s] else s, a)
                |> Map.ofArray

printfn "%A" vertices_with_edges