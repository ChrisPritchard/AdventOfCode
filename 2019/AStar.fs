module AStar

open System
open System.Collections.Generic

let astar<'T when 'T : equality> (isGoal: 'T -> bool) (edges: 'T -> seq<'T>) (d: 'T -> 'T -> float) (h: 'T -> float) (start: 'T) =
    
    let cameFrom = Dictionary<'T, 'T>()
    let gScore = Dictionary<'T, float>()
    gScore.Add (start, 0.)

    let fScore = Dictionary<'T, float>()
    fScore.Add (start, h start)

    let openSet = HashSet<'T>()
    openSet.Add start |> ignore

    let rec reconstructPath acc v =
        if cameFrom.ContainsKey v then
            reconstructPath (v::acc) cameFrom.[v]
        else
            v::acc

    let rec searcher () =
        if openSet.Count = 0 then 
            None
        else
            let current = Seq.minBy (fun v -> if fScore.ContainsKey v then fScore.[v] else Double.MaxValue) openSet
            if isGoal current then Some (reconstructPath [] current)
            else
                openSet.Remove current |> ignore
                let edges = edges current
                for neighbour in edges do
                    let tentativeGscore = gScore.[current] + d current neighbour
                    let found, currentGscore = gScore.TryGetValue neighbour
                    if not found || tentativeGscore < currentGscore then
                        cameFrom.Add(neighbour, current)
                        gScore.Add(neighbour, tentativeGscore)
                        fScore.Add(neighbour, tentativeGscore + h neighbour)
                        openSet.Add neighbour |> ignore
                searcher ()

    searcher ()

    // example of use:
    
    (*
    let testArray = 
        array2D
            [|
                [|' ';' ';' ';' ';' ';' ';' ';' ';' ';' '|]
                [|' ';' ';' ';' ';' ';' ';' ';' ';' ';' '|]
                [|' ';' ';' ';' ';' ';' ';' ';' ';' ';' '|]
                [|' ';' ';' ';' ';' ';' ';' ';'X';'X';' '|]
                [|' ';' ';' ';' ';' ';' ';'X';'X';' ';' '|]
                [|' ';' ';' ';' ';' ';' ';'X';' ';' ';' '|]
                [|' ';' ';' ';' ';'X';'X';'X';' ';'X';'X'|]
                [|' ';' ';' ';' ';'X';' ';' ';' ';'X';' '|]
                [|' ';' ';' ';' ';'X';' ';'X';'X';'X';' '|]
                [|' ';' ';' ';' ';'X';' ';'X';' ';' ';' '|]
                [|' ';' ';' ';' ';'X';' ';'X';' ';' ';' '|]
                [|' ';' ';' ';' ';'X';' ';' ';' ';'X';' '|]
                [|' ';' ';' ';' ';'X';' ';'X';'X';'X';' '|]
                [|' ';' ';' ';' ';'X';' ';'X';' ';' ';' '|]
                [|' ';' ';' ';' ';'X';' ';'X';' ';' ';' '|]
            |]
    
    let start = 0, 0
    let gx, gy = testArray.GetLength(1) - 1, testArray.GetLength(0) - 1
    let d _ _ = 1.
    let h (x, y) = sqrt ((float gx - float x) ** 2. + (float gy - float y) ** 2.)
    let isGoal = (=) (gx, gy)
    let edges (x, y) =
        [-1, 0; 1, 0; 0, -1; 0, 1] 
        |> Seq.map (fun (nx, ny) -> x + nx, y + ny)
        |> Seq.filter (fun (dx, dy) -> 
            dx >= 0 && dy >= 0 && dx < testArray.GetLength(1) && dy < testArray.GetLength(0)
            && testArray.[dy, dx] = ' ')
    
    let path = AStar.astar isGoal edges d h start |> Option.defaultValue []

    for y = 0 to testArray.GetLength(0) - 1 do
        for x = 0 to testArray.GetLength(1) - 1 do
            if Seq.contains (x, y) path then printf "*"
            else printf "%c" testArray.[y, x]
        printfn ""
*)