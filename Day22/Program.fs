open System

let geologicIndex p target erosionLevels =
    match p with
    | 0, 0 -> 0
    | _ when p = target -> 0
    | x, y when y = 0 -> x * 16807
    | x, y when x = 0 -> y * 48271
    | x, y -> 
        Map.find (x-1, y) erosionLevels 
        * Map.find (x, y-1) erosionLevels

let map target (w, h) depth =
    [0..w] |> List.collect (fun x ->
    [0..h] |> List.map (fun y -> x, y))
    |> List.fold (fun erosionLevels p -> 
        let g = geologicIndex p target erosionLevels
        let e = (g + depth) % 20183
        Map.add p e erosionLevels) Map.empty

type Tool = Torch | ClimbingGear | Neither

let bfs (sx, sy, st) goal erosionLevels = 

    let allowedTools x y erosionLevels =
        let tileType = Map.tryFind (x, y) erosionLevels |> Option.map (fun e -> e % 3)
        match tileType with
        | Some 0 -> [Torch;ClimbingGear]
        | Some 1 -> [ClimbingGear;Neither]
        | Some 2 -> [Neither;Torch]
        | _ -> []

    let neighbours x y tool erosionLevels closed = 
        [1,0;-1,0;0,1;0,-1]
        |> List.map (fun (dx, dy) -> x + dx, y + dy, tool)
        |> List.filter (fun (nx, ny, _) -> 
            allowedTools nx ny erosionLevels |> List.contains tool
            && Set.contains (nx, ny, tool) closed |> not)

    let rec processNext unprocessed toprocess closed =
        match unprocessed, toprocess with
        | [], [] -> failwith "no solution found"
        | [], tp -> processNext (List.rev tp) [] closed
        | (x, y, tool, switching, minutes)::rest, _ ->
            if switching > 0 then
                if switching <> 1 then
                    let newToProcess = (x, y, tool, switching - 1, minutes + 1)::toprocess
                    processNext rest newToProcess closed
                else if Set.contains (x, y, tool) closed then
                    processNext rest toprocess closed
                else
                    let newToProcess = (x, y, tool, switching - 1, minutes + 1)::toprocess
                    let newClosed = Set.add (x, y, tool) closed
                    processNext rest newToProcess newClosed
            else if (x, y, tool) = goal then minutes
            else
                let neighbours = neighbours x y tool erosionLevels closed
                let adjacent = 
                    neighbours
                    |> List.map (fun (x, y, t) -> (x, y, t, 0, minutes + 1))
                let newClosed = 
                    neighbours |> Set.ofList |> Set.union closed
                let switch = 
                    allowedTools x y erosionLevels
                    |> List.except [tool]
                    |> List.map (fun t -> x, y, t, 6, minutes + 1)
                    |> List.head
                processNext rest (switch::(adjacent @ toprocess)) newClosed

    processNext [sx, sy, st, 0, 0] [] (Set.empty.Add (0, 0, Torch))

[<EntryPoint>]
let main _ =
    // let depth = 510
    // let (tx, ty) = 10,10
    // let mapSize = 15,15

    let depth = 10914
    let (tx, ty) = 9,739
    let mapSize = 18,1500

    let riskLevel = 
        map (tx, ty) (tx, ty) depth        
        |> Map.toList 
        |> List.sumBy (snd >> fun e -> e % 3)

    printfn "part 1: %i" riskLevel

    let riskLevel2 =
        map (tx, ty) mapSize depth
    let part2 = bfs (0, 0, Torch) (tx, ty, Torch) riskLevel2

    printfn "part 2: %i" part2

    0