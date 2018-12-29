﻿open System

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

let astarConfig map: AStar.Config<int * int * Tool> = {
    maxIterations = None
    neighbours = fun (x, y, _) ->
        [-1,0;1,0;0,-1;0,1]
        |> List.map (fun (dx, dy) ->  
            Map.tryFind (x + dx, y + dy) map
            |> Option.map (fun e ->
                let ox, oy = x + dx, y + dy
                match e % 3 with
                | 0 -> 
                    [ox, oy, ClimbingGear; ox, oy, Torch]
                | 1 -> 
                    [ox, oy, ClimbingGear; ox, oy, Neither]
                | _ -> 
                    [ox, oy, Torch; ox, oy, Neither]))
        |> List.choose id 
        |> List.collect id
        |> List.toSeq
    fCost = fun (x, y, _) (gx, gy, _) ->
        sqrt ((float gx - float x)**2. + (float gy - float y)**2.)
    gCost = fun (_, _, t1) (_, _, t2) -> if t1 = t2 then 1. else 8.
}

[<EntryPoint>]
let main _ =
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
    let path = 
        AStar.search 
            (0, 0, Torch) (tx, ty, Torch) 
            (astarConfig riskLevel2)
        |> Option.map Seq.toList

    let part2 =
        match path with
        | None -> failwith "no path found for part 2"
        | Some p ->

            // [0..ty+5] |> List.map (fun y ->
            // [0..tx+5] |> List.map (fun x -> 
            //     if x = 0 && y = 0 then "M"
            //     else if x = tx && y = ty then "T"
            //     else if List.tryFind (fun (px, py, _) -> px = x && py = y) p <> None then "X"
            //     else
            //     match riskLevel2.[x, y] % 3 with
            //     | 0 -> "."
            //     | 1 -> "="
            //     | _ -> "|") |> String.concat "")
            // |> List.iter (printfn "%s")

            p 
            |> List.rev 
            |> List.fold (fun (last, total) (_, _, next) ->
                match last with
                | None -> Some next, 0
                | Some t when t = next -> Some next, total + 1
                | _ -> Some next, total + 8) (None, 0)
            |> snd

    printfn "part 2: %i" part2

    0