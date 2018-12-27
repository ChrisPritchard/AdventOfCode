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

let astarConfig map: AStar.Config<int * int * int> = {
    maxIterations = None
    neighbours = fun (x, y, _) ->
        [-1,0;1,0;0,-1;0,1]
        |> List.map (fun (dx, dy) -> x + dx, y + dy, Map.tryFind (x + dx, y + dy) map)
        |> List.choose (fun (_, _, tile) -> tile)
        |> List.toSeq
    fCost = fun (x, y, _) (gx, gy, _) ->
        sqrt ((float gx - float x)**2. + (float gy - float y)**2.)
    gCost = fun (x, y, _) (gx, gy, _) -> 1.
}

[<EntryPoint>]
let main _ =
    let depth = 10914
    let target = 9,739

    let riskLevel = 
        map target depth        
        |> Map.toList 
        |> List.sumBy (snd >> fun e -> e % 3)

    printfn "part 1: %i" riskLevel

    0
