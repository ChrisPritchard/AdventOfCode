module Day15

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

    let tryDirection ((x, y), map, moved, result) (position, input) =
        match moved, result with
        | true, _ | _, Some _ -> ((x, y), map, moved, result)
        | false, None ->
            io.write input
            Intcode.run 0L 0L mem io |> ignore
            match io.read () with
            | _, 0L -> 
                let newMap = Map.add position '#' map
                ((x, y), newMap, false, result)
            | _, 1L -> 
                let newMap = Map.add position '.' map
                (position, newMap, true, result)
            | _, 2L -> 
                let newMap = Map.add position '.' map
                let result = findResult position newMap
                (position, newMap, true, Some result)
            | _ -> failwith "invalidstate"
                
    let rec runner (x, y) map path = 
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
                io.write (dirs |> List.find (fst >> (=) last) |> snd)
                Intcode.run 0L 0L mem io |> ignore
                io.read () |> ignore
                last, map, rem, None
            | _ -> failwith "no path"

        let pos, map, path, result = 
            if List.length options = 0 then
                backtrack ()
            else
                let (nextPos, nextMap, _, result) =
                    (((x, y),  map, false, None), options)
                    ||> List.fold tryDirection
                if nextPos = (x, y) then
                    backtrack ()
                else
                    nextPos, nextMap, ((x, y)::path), result

        match result with
        | Some n -> n
        | None ->
            runner pos map path

    let startMap = Map.empty.Add ((0, 0), '.')
    runner (0, 0) startMap []
        

let part2 () =

    let io = Intcode.IO.create ()
    let mem = Intcode.memFrom input

    0
    