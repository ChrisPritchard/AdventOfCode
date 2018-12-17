﻿open System
open System.IO
open AStar

type Fighter = {
    mutable x: int; mutable y: int; 
    mutable health: int
    kind: FighterKind
} 
with member __.Pos = __.x, __.y
and FighterKind = Goblin | Elf

let attack = 3

let create x y kind = { x = x; y = y; health = 200; kind = kind }

let gScore (_, y1) (_, y2) = 
    if y1 = y2 then 0.9
    else 1.
let fScore (x, y) (gx, gy) = 
    sqrt ((float gx - float x)**2. + (float gy - float y)**2.)

let render walls fighters = ()
    // Console.CursorVisible <- false
    // System.Threading.Thread.Sleep 1000
    // Console.Clear ()
    // Console.CursorLeft <- 0
    // for (x, y) in walls do
    //     Console.CursorLeft <- x
    //     Console.CursorTop <- y
    //     Console.Write '#'
    // for f in fighters do
    //     Console.CursorLeft <- f.x
    //     Console.CursorTop <- f.y
    //     Console.Write (if f.kind = Goblin then 'G' else 'E')


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

    let neighbours goal (x, y) =
        [(-1,0);(1,0);(0,-1);(0,1)] 
        |> Seq.map (fun (dx, dy) -> x + dx, y + dy)
        |> Seq.filter (fun  (nx, ny) ->
            match 
                Set.contains (nx, ny) walls, 
                List.tryFind (fun (f : Fighter) -> (nx, ny) <> goal && f.x = nx && f.y = ny) fighters with
            | false, None -> true
            | _ -> false)

    let path fighter goal = 
        AStar.search (fighter.x, fighter.y) goal { neighbours = neighbours goal; gCost = gScore; fCost = fScore; maxIterations = Some 10 }

    let advance others fighter =
        let enemyKind = match fighter.kind with Elf -> Goblin | _ -> Elf
        let opponent = 
            others 
            |> List.filter (fun f -> f.kind = enemyKind && f.health > 0) 
            |> List.map (fun e -> 
                match path fighter e.Pos with
                | Some p -> 
                    Some (p |> Seq.rev |> Seq.toList, e)
                | None -> None)
            |> List.choose id
            |> List.sortBy (fun (p, _) -> 
                match p with (_::(x, y)::_) -> List.length p, y, x | _ -> 100,100,100)
            |> List.tryHead
        match opponent with
        | Some ([_;_], e) -> 
            e.health <- e.health - attack
        | Some (_::(x, y)::_, _) -> 
            fighter.x <- x
            fighter.y <- y
        | _ -> ()
        fighter

    let rec game walls fighters turn =
        render walls fighters
        let next =
            fighters 
            |> List.sortBy (fun f -> f.y, f.x)
            |> List.map (fun f -> if f.health > 0 then advance fighters f else f)
            |> List.filter (fun f -> f.health > 0)
        match next |> List.groupBy (fun f -> f.kind) |> List.length with
        | 1 -> turn
        | _ -> game walls next (turn + 1)

    printfn "part 1: %i" <| game walls fighters 0
    0
