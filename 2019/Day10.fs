module Day10

open System.IO

let input = File.ReadAllLines ("./inputs/day10.txt")
let asteroids = 
    input 
    |> Seq.mapi (fun y line ->
        line |> Seq.mapi (fun x cell -> (x, y), cell = '#') |> Seq.filter snd |> Seq.map fst)
    |> Seq.collect id
    |> Seq.toArray
    
let part1 () =

    let simplified (dx, dy) =
        if dx = 0 then 0, dy / abs dy
        elif dy = 0 then dx / abs dx, 0
        elif dx > dy && dx % dy = 0 then dx / dy, dy / abs dy
        elif dy > dx && dy % dx = 0 then dx / abs dx, dy / dx
        elif dx = dy then dx / abs dx, dy / abs dy
        else dx, dy

    let linesOfSight (ox, oy) (visible, blocked) (x, y) =
        if Set.contains (x, y) blocked || (ox, oy) = (x, y) then (visible, blocked)
        else
            let dx, dy = simplified (x - ox, y - oy)
            let rec ray (visible, blocked) (nx, ny) =
                if nx < 0 || ny < 0 || nx >= input.[0].Length || ny >= input.Length then
                    visible, blocked
                else
                    ray (Set.remove (nx, ny) visible, Set.add (nx, ny) blocked) (nx + dx, ny + dy)
            ray (Set.add (x, y) visible, blocked) (x + dx, y + dy)

    let visible (x, y) =
        ((Set.empty, Set.empty), asteroids) ||> Array.fold (linesOfSight (x, y))
        |> fst |> fun visible -> (x, y), visible
    
    asteroids |> Seq.map (visible >> snd >> Set.count) |> Seq.max

let part2 () =
    
    0