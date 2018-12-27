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

let map (tx, ty) depth =
    [0..tx] |> List.collect (fun x ->
    [0..ty] |> List.map (fun y -> x, y))
    |> List.fold (fun erosionLevels p -> 
        let g = geologicIndex p (tx, ty) erosionLevels
        let e = (g + depth) % 20183
        Map.add p e erosionLevels) Map.empty

type Tool = Torch | ClimbingGear | Neither

let astarConfig map: AStar.Config<int * int * Tool> = {
    maxIterations = None
    neighbours = fun (x, y, tool) ->
        [-1,0;1,0;0,-1;0,1]
        |> List.map (fun (dx, dy) ->  
            Map.tryFind (x + dx, y + dy) map
            |> Option.map (fun e ->
                let ox, oy = x + dx, y + dy
                match e % 3, tool with
                | 0, Neither -> 
                    [ox, oy, ClimbingGear; ox, oy, Torch]
                | 1, Torch -> 
                    [ox, oy, ClimbingGear; ox, oy, Neither]
                | 2, ClimbingGear -> 
                    [ox, oy, Torch; ox, oy, Neither]
                | _ -> 
                    [ox, oy, tool]))
        |> List.choose id 
        |> List.collect id
        |> List.toSeq
    fCost = fun (x, y, _) (gx, gy, _) ->
        sqrt ((float gx - float x)**2. + (float gy - float y)**2.)
    gCost = fun (_, _, t1) (_, _, t2) -> if t1 = t2 then 1. else 7.
}

[<EntryPoint>]
let main _ =
    let depth = 10914
    let (tx, ty) = 9,739

    let riskLevel = 
        map (tx, ty) depth        
        |> Map.toList 
        |> List.sumBy (snd >> fun e -> e % 3)

    printfn "part 1: %i" riskLevel

    let riskLevel2 =
        map (tx*2, ty*2) depth
    let path = 
        AStar.search 
            (0, 0, Torch) (tx, ty, Torch) 
            (astarConfig riskLevel2)



    0
