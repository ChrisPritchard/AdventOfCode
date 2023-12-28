let input = System.IO.File.ReadAllLines "input.txt"

// solution is a recreation of the excellent https://github.com/ricbit/advent-of-code/blob/main/2023/adv17-r.py
// basically a*, but with a complicated approach to the neighbours algorithm

let start = 0, 0
let goal = input[input.Length - 1].Length - 1, input.Length - 1
let is_valid (x, y) = x >= 0 && y >= 0 && y < input.Length && x < input[y].Length
let m (x, y) = int (input[y][x]) - int '0'
type Direction = Vertical | Horizontal | Start

let heuristic (x, y) =
    let gx, gy = goal in (gx - x) + (gy - y)

let search min_size max_size = 
    let mutable visited = Map.empty
    let get_visited pos dir = if Map.containsKey (pos, dir) visited then visited[pos, dir] else System.Int32.MaxValue
    let vnext = System.Collections.Generic.PriorityQueue()
    vnext.Enqueue ((heuristic start, 0, start, Start), heuristic start) // fscore, dscore, pos, dir, sorted by fscore (heuristic)

    let rec proceed () =
        if vnext.Count = 0 then
            None
        else
            let old_heuristic, score, (x, y), direction = vnext.Dequeue ()
            if old_heuristic > get_visited (x, y) direction then
                proceed ()
            else if (x, y) = goal then Some score
            else
                for dx, dy in [ -1, 0; 1, 0; 0, -1; 0, 1 ] do
                    if direction = Start || (dx = 0 && direction = Horizontal) || (dy = 0 && direction = Vertical) then
                        let mutable dscore = 0
                        let mutable cont = true
                        for size in 1..max_size do
                            if cont then
                                let nx, ny = x + (size * dx), y + (size * dy)
                                if not (is_valid (nx, ny)) then
                                    cont <- false
                                else 
                                    dscore <- dscore + m (nx, ny)
                                    let pure_score = score + dscore
                                    let new_heuristic = heuristic (nx, ny) + pure_score
                                    let new_dir = if dy = 0 then Horizontal else Vertical
                                    if size >= min_size && new_heuristic < get_visited (nx, ny) new_dir then 
                                        vnext.Enqueue ((new_heuristic, pure_score, (nx, ny), new_dir), new_heuristic)
                                        visited <- Map.add ((nx, ny), new_dir) new_heuristic visited
                proceed ()
    proceed ()

let part1 = search 1 3
printfn "Part 1: %d" part1.Value
let part2 = search 4 10
printfn "Part 2: %d" part2.Value