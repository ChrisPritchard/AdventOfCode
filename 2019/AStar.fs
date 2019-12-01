module AStar

open System
open System.Collections.Generic

let astar<'T when 'T : equality> (isGoal: 'T -> bool) (edges: 'T -> seq<'T * float>) (h: 'T -> float) (start: 'T) =
    
    let cameFrom = Dictionary<'T, 'T>()
    let gScore = Dictionary<'T, float>()
    gScore.Add(start, 0.)

    let fScore = Dictionary<'T, float>()
    fScore.Add(start, h start)

    let comparer a b =
        let fA = fScore.TryGetValue a |> fun (exists, v) -> if exists then v else Double.MaxValue
        let fB = fScore.TryGetValue b |> fun (exists, v) -> if exists then v else Double.MaxValue
        fA - fB |> int
    let sorter = Comparer<'T>.Create(Comparison<'T>(comparer))
    let openSet = SortedSet<'T>(sorter)
    openSet.Add (start) |> ignore

    let rec reconstructPath acc v =
        if cameFrom.ContainsKey v then
            reconstructPath (v::acc) cameFrom.[v]
        else
            v::acc

    let rec searcher () =
        if openSet.Count = 0 then None
        else
            let current = Seq.head openSet
            if isGoal current then Some (reconstructPath [] current)
            else
                openSet.Remove current |> ignore
                let edges = edges current
                for (neighbour, d) in edges do
                    let tentativeGscore = gScore.[current] + d
                    let found, currentGscore = gScore.TryGetValue neighbour
                    if not found || tentativeGscore < currentGscore then
                        cameFrom.Add(neighbour, current)
                        gScore.Add(neighbour, tentativeGscore)
                        fScore.Add(neighbour, tentativeGscore + h neighbour)
                        openSet.Add neighbour |> ignore
                searcher ()

    searcher ()