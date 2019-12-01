module BFS

open System.Collections.Generic

let bfs<'T when 'T : equality> (isGoal: 'T -> bool) (edges: 'T -> seq<'T>) (start: 'T) =
    let queue = Queue<'T>()
    let discovered = HashSet<'T>()
    let parents = Dictionary<'T, 'T>()
    
    discovered.Add start |> ignore
    queue.Enqueue start

    let rec reconstructPath acc v =
        if parents.ContainsKey v then
            reconstructPath (v::acc) parents.[v]
        else
            v::acc

    let rec searcher () =
        if queue.Count = 0 then None
        else
            let v = queue.Dequeue ()
            if isGoal v then
                reconstructPath [] v |> Some
            else
                let edges = edges v
                for w in edges do
                    if not (discovered.Contains w) then
                        discovered.Add w |> ignore
                        parents.Add (w, v)
                        queue.Enqueue w
                searcher ()
            
    searcher ()

let testData = [
    (1,2);  (1,3);  (1,4)
    (2,5);  (2,6)
    (3,7);
    (4,8);  (4,9); (4,10);  (4,11)
    (5,8);  (5,12)
    (6,11);
    (7,12); (7,13); (7,15)
    ]

let isGoal = (=) 15
let edges v =
     testData |> Seq.filter (fst >> (=) v) |> Seq.map snd

let result = bfs<int> isGoal edges 1