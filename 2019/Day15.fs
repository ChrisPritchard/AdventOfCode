﻿module Day15

open System.IO

let input = (File.ReadAllText ("./inputs/day15.txt")).Split ','

let part1 () =

    let io = Intcode.IO.create ()
    let mem = Intcode.memFrom input
    
    let findResult target map = 
        let edges (x, y) = 
            [
                x - 1, y
                x + 1, y
                x, y - 1
                x, y + 1
            ] |> Seq.filter (fun (x, y) -> (Map.tryFind (x, y) map |> Option.defaultValue '#') <> '#')
        BFS.bfs ((=) target) edges (0, 0) |> Option.defaultValue [] |> Seq.length

    let tryDirection ((x, y), ip, rb, mem, map, moved, result) (position, input) =
        match moved, result with
        | true, _ | _, Some _ -> ((x, y), ip, rb, mem, map, moved, result)
        | false, None ->
            io.write input
            let _, ip, rb, mem = Intcode.run ip rb mem io
            match io.read () with
            | _, 0L -> 
                let newMap = Map.add position '#' map
                ((x, y), ip, rb, mem, newMap, false, result)
            | _, 1L -> 
                let newMap = Map.add position '.' map
                (position, ip, rb, mem, newMap, true, result)
            | _, 2L -> 
                let newMap = Map.add position '.' map
                let result = findResult position newMap
                (position, ip, rb, mem, newMap, true, Some result)
            | _ -> failwith "invalidstate"
                
    let rec runner (x, y) ip rb mem map path = 
        let dirs = 
            [
                (x, y - 1), 1L
                (x, y + 1), 2L
                (x - 1, y), 3L
                (x + 1, y), 4L
            ]
        let options = dirs |> List.filter (fun (t, _) -> Map.containsKey t map |> not)

        let backtrack () =
            match path with
            | last::rem ->
                let nextMap = Map.add (x, y) '#' map
                io.write (dirs |> List.find (fst >> (=) last) |> snd)
                let _, ip, rb, mem = Intcode.run ip rb mem io
                io.read () |> ignore
                last, ip, rb, mem, nextMap, rem, None
            | _ -> failwith "no path"

        let pos, ip, rb, mem, map, path, result = 
            if List.length options = 0 then
                backtrack ()
            else
                let (nextPos, ip, rb, mem, nextMap, _, result) =
                    (((x, y), ip, rb, mem, map, false, None), options)
                    ||> List.fold tryDirection
                if nextPos = (x, y) then
                    backtrack ()
                else
                    nextPos, ip, rb, mem, nextMap, ((x, y)::path), result

        match result with
        | Some n -> n
        | None ->
            runner pos ip rb mem map path

    let startMap = Map.empty.Add ((0, 0), '.')
    runner (0, 0) 0L 0L mem startMap []
        

let part2 () =

    let io = Intcode.IO.create ()
    let mem = Intcode.memFrom input

    0
    