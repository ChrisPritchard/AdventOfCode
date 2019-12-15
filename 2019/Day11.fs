module Day11

open Common
open System.IO

let input = (File.ReadAllText ("./inputs/day11.txt")).Split ','
    
let turnLeft (dx, dy) =
    if dx = 1 then 0, -1
    elif dx = -1 then 0, 1
    elif dy = 1 then 1, 0
    else -1, 0

let turnRight (dx, dy) =
    if dx = 1 then 0, 1
    elif dx = -1 then 0, -1
    elif dy = 1 then -1, 0
    else 1, 0

let rec painter (map: Map<int * int, int64>) (x, y) dir (io: Intcode.IO) (_, ip, rb, mem) =
    let currentTile = Map.tryFind (x, y) map |> Option.defaultValue 0L
    io.write currentTile

    let (state, ip, rb, mem) = Intcode.run ip rb mem io

    let map = 
        let _, colour = io.read ()
        Map.add (x, y) colour map

    let (dx, dy) = 
        let _, direction = io.read ()
        if direction = 0L then
            turnLeft dir
        else
            turnRight dir

    let pos = (x + dx, y + dy)
    if state = Intcode.Halted then 
        map 
    else
        painter map pos (dx, dy) io (state, ip, rb, mem)
        
let part1 () =    
    
    let mem = Intcode.memFrom input
    let io = Intcode.IO.create ()

    let final = painter Map.empty (0, 0) (0, -1) io (Intcode.Running, 0L, 0L, mem)
    final.Count

let part2 () =
    
    let mem = Intcode.memFrom input
    let io = Intcode.IO.create ()
    
    let final = painter (Map.empty.Add ((0, 0), 1L)) (0, 0) (0, -1) io (Intcode.Running, 0L, 0L, mem)
    
    asString <| seq {
        yield '\n'
        for y = 0 to 6 do
            for x = 0 to 40 do
                yield 
                    Map.tryFind (x, y) final |> Option.defaultValue 0L |> function n when n = 0L -> ' ' | _ -> '#'
            yield '\n'
    }