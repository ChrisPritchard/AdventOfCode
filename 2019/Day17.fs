module Day17

open Common
open System.IO

let input = (File.ReadAllText ("./inputs/day17.txt")).Split ','

let mem = Intcode.memFrom input
let io = Intcode.IO.create ()
Intcode.run 0L 0L mem io |> ignore

let rec mapper acc line = 
    let canRead, next = io.read ()
    if not canRead then List.rev acc |> List.toArray
    else
        if next = 10L then
            let line = List.rev line |> List.toArray
            let acc = if line.Length > 0 then line::acc else acc
            mapper acc []
        else
            mapper acc (char next::line)
            
let map = mapper [] []

let part1 () =

    for y = 0 to map.Length - 1 do
        for x = 0 to map.[y].Length - 1 do
            printf "%c" map.[y].[x]
        printfn ""

    let intersection x y =  
        if map.[y].[x] <> '#' then None
        else
            let isIntersection =
                [
                    -1, 0
                    1, 0
                    0, -1
                    0, 1
                ] 
                |> Seq.map (fun (dx, dy) -> x + dx, y + dy)
                |> Seq.filter (fun (x, y) -> x >= 0 && y >= 0 && x < map.[0].Length && y < map.Length)
                |> Seq.filter (fun (x, y) -> map.[y].[x] = '#')
                |> Seq.length >= 3
            if isIntersection then Some (y * x) else None

    seq {
        for y = 0 to map.Length - 1 do
            for x = 0 to map.[y].Length - 1 do
                match intersection x y with
                | Some n -> yield n
                | None -> ()
    } |> Seq.sum

let part2 () =

    let start = (3, 0)
    let goal = (24, 34)

    let edges ((x, y), (lx, ly)) =
        let options = 
            [
                -1, 0
                1, 0
                0, -1
                0, 1
            ] 
            |> Seq.map (fun (dx, dy) -> x + dx, y + dy)
            |> Seq.filter (fun (x, y) -> x >= 0 && y >= 0 && x < map.[0].Length && y < map.Length)
            |> Seq.filter (fun (x, y) -> map.[y].[x] = '#' && (x, y) <> (lx, ly))
            |> Seq.toArray
        let straight = 
            Array.tryFind (fun (x, y) -> x = lx || y = ly) options
        seq {
            match straight with
            | Some o -> yield o, (x, y)
            | None ->
                yield! options |> Seq.map (fun o -> o, (x, y))
        }

    let isGoal ((x, y), _) = (x, y) = goal

    let fullPath = BFS.run isGoal edges (start, start) |> Option.defaultValue []
    let translated = 
        ((['R'], 0), fullPath.[1..]) 
        ||> List.fold (fun (acc, cnt) ((x, y), (lx, ly)) ->
            if lx = x || ly = y)

    // calculate full path
        // find start and end
        // bfs to get path
            // returns n and turns
            // on intersections goes straight
    // group by runs

    0