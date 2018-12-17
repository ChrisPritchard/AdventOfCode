open System
open System.IO

type Fighter = {
    x: int; 
    y: int; 
    health: int
    attack: int
    kind: FighterKind
} 
and FighterKind = Goblin | Elf

let create x y kind = { x = x; y = y; health = 200; attack = 3; kind = kind }

let render walls fighters = //()
    Console.CursorVisible <- false
    System.Threading.Thread.Sleep 500
    Console.Clear ()
    Console.CursorLeft <- 0
    for (x, y) in walls do
        Console.CursorLeft <- x
        Console.CursorTop <- y
        Console.Write '#'
    for f in fighters do
        Console.CursorLeft <- f.x
        Console.CursorTop <- f.y
        Console.Write (if f.kind = Goblin then 'G' else 'E')

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
            | 'G' -> w, (create x y Goblin)::f
            | 'E' -> w, (create x y Elf)::f
            | _ -> w, f) (Set.empty, [])

    let rec game walls fighters turn =
        0, 0

    printfn "part 1: turn %i health %i" <|| game walls fighters 0
    0
