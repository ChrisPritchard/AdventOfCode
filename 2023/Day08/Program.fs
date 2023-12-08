let input = Input.value

let lines = input.Split [|'\n'|]

let instructions = lines[0].ToCharArray()
let map = lines |> Array.skip 2 |> Array.map (fun s -> 
    let parts = s.Split ([|' ';'=';'(';',';')'|], System.StringSplitOptions.RemoveEmptyEntries)
    parts[0], (parts[1], parts[2])) |> Map.ofArray

let rec follow_route index count pos = 
    let options = map[pos]
    let next = if instructions[index] = 'L' then fst options else snd options
    if next = "ZZZ" then count + 1
    else
        let next_index = index + 1
        if next_index = instructions.Length then
            follow_route 0 (count + 1) next
        else
            follow_route next_index (count + 1) next

let path_count = follow_route 0 0 "AAA"
printfn "Part 1: %d" path_count

let starting_nodes = map.Keys |> Seq.toArray |> Array.filter (fun k -> k[2] = 'A')

let rec find_z_dist index count pos = 
    let options = map[pos]
    let next = if instructions[index] = 'L' then fst options else snd options
    if next[2] = 'Z' then count + 1
    else
        let next_index = index + 1
        if next_index = instructions.Length then
            find_z_dist 0 (count + 1) next
        else
            find_z_dist next_index (count + 1) next

let cycles = starting_nodes |> Array.map (fun start -> find_z_dist 0 0 start |> uint64)

let rec gcd a b = if b = 0UL then a else gcd b (a % b)
let lcm = cycles |> Array.reduce (fun a b -> (a * b) / gcd a b)

printfn "Part 2: %d" lcm