let input = System.IO.File.ReadAllLines "input.txt"

let edges (x, y) =
    [|-1,0; 1,0; 0,-1; 0,1|] |> Array.choose (fun (dx, dy) -> 
        let ox, oy = x + dx, y + dy
        if ox < 0 || oy < 0 || oy >= input.Length || ox >= input[oy].Length then None
        else
            let c = input[oy][ox]
            if c = '.' then Some (ox, oy)
            else if c = '>' then Some (x + 2, y)
            else if c = 'v' then Some (x, y + 2)
            else None)

let start = (1, 0)
let target = (input[0].Length - 2, input.Length - 1)

let rec expand_paths ongoing longest = 
    let new_paths = ongoing |> Array.collect (fun (last, acc) -> 
        let neighbours = edges last
        neighbours |> Array.choose (fun n -> if Set.contains n acc then None else Some (n, Set.add n acc)))
    let finished = Array.filter (fun (last, _) -> last = target) new_paths
    let longest = if finished.Length > 0 then max longest <| Array.max (Array.map (snd >> Set.count) finished) else longest
    let ongoing = Array.except finished new_paths
    if ongoing.Length > 0 then
        expand_paths ongoing longest
    else longest

let part1 = expand_paths [|start, Set.empty.Add start|] 0
printfn "Part 1: %d" part1