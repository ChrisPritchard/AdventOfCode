module Day15

open System.IO

let input = (File.ReadAllText ("./inputs/day15.txt")).Split ','

type MoveAttempt = NotMoved | FoundEnd of int | NextMove of (int * int)

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
       
    let rec runner (x, y) map path = 
        let dirs = 
            [
                (x, y - 1), 1L, 2L
                (x, y + 1), 2L, 1L
                (x - 1, y), 3L, 4L
                (x + 1, y), 4L, 3L
            ]
        let options = dirs |> List.filter (fun (t, _, _) -> 
            if List.contains t path then false
            else
                if Map.containsKey t map then map.[t] <> '#' else true)

        let forinput n = 
            io.write n
            Intcode.run 0L 0L mem io |> ignore
            io.read () |> snd

        let backtrack () =
            match path with
            | last::rem ->
                let n = dirs |> List.pick (fun (t, i, _) -> if t = last then Some i else None)
                forinput n |> ignore
                let nextMap = Map.add (x, y) '#' map
                last, nextMap, rem, None
            | _ -> failwith "no path"

        let tryDirection (map, result) (position, input, reverse) =
            match result with
            | FoundEnd _ -> (map, result)
            | _ ->
                match forinput input with
                | 0L -> 
                    let newMap = Map.add position '#' map
                    (newMap, result)
                | 1L -> 
                    let newMap = Map.add position '.' map
                    forinput reverse |> ignore
                    (newMap, NextMove position)
                | 2L -> 
                    let newMap = Map.add position '.' map
                    let pathLength = findResult position newMap
                    (newMap, FoundEnd pathLength)
                | _ -> failwith "invalidstate"

        let pos, map, path, result = 
            if List.length options = 0 then
                backtrack ()
            else
                let (nextMap, result) =
                    ((map, NotMoved), options)
                    ||> List.fold tryDirection
                match result with
                | NotMoved -> backtrack ()
                | NextMove nextPos ->
                    let n = dirs |> List.pick (fun (t, i, _) -> if t = nextPos then Some i else None)
                    forinput n |> ignore
                    nextPos, nextMap, ((x, y)::path), None
                | FoundEnd pathLength ->
                    (x, y), nextMap, path, Some pathLength

        match result with
        | Some n -> n
        | None ->
            printfn "%A" pos
            System.Threading.Thread.Sleep 50
            runner pos map path

    let startMap = Map.empty.Add ((0, 0), '.')
    runner (0, 0) startMap []
        

let part2 () =

    let io = Intcode.IO.create ()
    let mem = Intcode.memFrom input

    0
    