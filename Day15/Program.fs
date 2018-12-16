open System.IO

type Fighter = {
    pos: int * int
    attack: int
    health: int
}

let create pos = { pos = pos; attack = 3; health = 200 }

[<EntryPoint>]
let main _ =
    let input = File.ReadAllLines "input.txt"

    let walls, goblins, elves = 
        input 
        |> Seq.mapi (fun y -> Seq.mapi (fun x c -> (x, y), c))
        |> Seq.collect id
        |> Seq.fold (fun (w, g, e) (p,c) ->
            match c with
            | '#' -> Set.add p w, g, e
            | 'G' -> w, (create p)::g, e
            | 'E' -> w, g, (create p)::e
            | _ -> w, g, e) (Set.empty, [], [])

    0
