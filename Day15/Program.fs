open System.IO

type Fighter = 
    | Goblin of x: int * y: int * health: int 
    | Elf of x: int * y: int * health: int 

let startHealth = 200

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

    0
