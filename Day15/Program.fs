open System.IO
open AStar

type Fighter = 
    | Goblin of x: int * y: int * health: int 
    | Elf of x: int * y: int * health: int 

let startHealth = 200

let gScore _ _ = 1.
let fScore (x, y) (gx, gy) = 
    sqrt ((float gx - float x)**2. + (float gy - float y)**2.)

[<EntryPoint>]
let main _ =
    let input = File.ReadAllLines "input.txt"

    let walls, fighters = 
        input 
        |> Seq.mapi (fun y -> Seq.mapi (fun x c -> x, y, c))
        |> Seq.collect id
        |> Seq.fold (fun (w, f) (x, y, c) ->
            match c with
            | '#' -> Set.add (x, y) w, f
            | 'G' -> w, (Goblin (x, y, startHealth))::f
            | 'E' -> w, (Elf (x, y, startHealth))::f
            | _ -> w, f) (Set.empty, [])

    let ordered fighters = 
        fighters |> List.sortBy (function 
            | Goblin (x, y, _) -> y, x 
            | Elf (x, y, _) -> y, x)
    
    let samePos (x, y) = 
        function 
        | Goblin (ox, oy, _) -> x = ox && y = oy
        | Elf (ox, oy, _) -> x = ox && y = oy

    let neighbours (x, y) =
        [(-1,0);(1,0);(0,-1);(0,1)] |> Seq.filter (fun (dx, dy) ->
            let nx, ny = x + dx, y + dy
            match 
                Set.contains (nx, ny) walls, 
                List.tryFind (samePos (nx, ny)) fighters with
            | false, None -> true
            | _ -> false)

    let path start goal = 
        AStar.search start goal 
        <| { neighbours = neighbours; gCost = gScore; fCost = fScore; maxIterations = None }

    0
