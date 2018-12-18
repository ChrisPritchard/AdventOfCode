﻿open System
open System.IO
open AStar

type Fighter = {
    x: int; y: int; 
    health: int
    attack: int
    kind: FighterKind
} 
with member __.Pos = __.x, __.y
and FighterKind = Goblin | Elf

let create x y kind = { x = x; y = y; health = 200; attack = 3; kind = kind }

let render walls fighters turn (width, height) =
    System.Threading.Thread.Sleep 100

    for y = 0 to height-1 do
        Console.CursorTop <- y   
        for x = 0 to width-1 do
            Console.CursorLeft <- x         
            let c =
                if Set.contains (x, y) walls then '#'
                else
                    match Array.tryFind (fun e -> e.health > 0 && e.x = x && e.y = y) fighters with
                    | Some f when f.kind = Goblin -> 'G'
                    | Some f when f.kind = Elf -> 'E'
                    | _ -> '.'
            Console.Write c
        Console.CursorLeft <- width + 1
        Console.Write "                                    "

    for row in fighters |> Seq.where (fun f -> f.health > 0) |> Seq.groupBy (fun f -> f.y) do
        for (i, f) in snd row |> Seq.sortBy (fun f -> f.x) |> Seq.mapi (fun i f -> i, f) do
        Console.CursorTop <- f.y
        Console.CursorLeft <- (width + 2) + i * 8
        Console.Write (sprintf "%s (%i)" (if f.kind = Goblin then "G" else "E") f.health)

    Console.CursorTop <- height + 1
    Console.CursorLeft <- 2
    Console.Write (sprintf "turn %i" turn)
    Console.ReadKey true

let getAstarConfig ignored fighters walls = {
        neighbours = fun (x, y) -> 
            [-1,0;1,0;0,-1;0,1]
            |> List.map (fun (dx, dy) -> x + dx, y + dy)
            |> List.filter (fun p -> not <| Set.contains p walls)
            |> List.filter (fun p -> 
                Seq.tryFind (fun (e : Fighter) -> e.health > 0 && e.Pos = p) 
                    (Array.except ignored fighters) = None)
            |> Seq.ofList
        fCost = fun _ _ -> 0.
        gCost = fun (_, y1) (_, y2) -> if y1 = y2 then 0.5 else 1.
        maxIterations = None
    }

let findPath (fighter : Fighter) fighters walls (index, (enemy : Fighter)) =
    let config = getAstarConfig [fighter;enemy] fighters walls
    match AStar.search fighter.Pos enemy.Pos config with
    | Some path -> 
        path 
        |> Seq.rev |> Seq.toList
        |> fun p -> Some (p.[1..], enemy, index)
    | _ -> None

[<EntryPoint>]
let main _ =
    let input = File.ReadAllLines "input.txt"

    let walls, start = 
        input 
        |> Seq.mapi (fun y -> Seq.mapi (fun x c -> x, y, c))
        |> Seq.collect id
        |> Seq.fold (fun (w, f) (x, y, c) ->
            match c with
            | '#' -> Set.add (x, y) w, f
            | 'G' -> w, (create x y Goblin)::f
            | 'E' -> w, (create x y Elf)::f
            | _ -> w, f) (Set.empty, [])

    let mutable gameOver = false
    let mutable turn = 0
    let mutable index = 0;
    let mutable fighters = start |> List.sortBy (fun f -> f.y, f.x) |> List.toArray

    Console.Clear ()
    Console.CursorVisible <- false

    while not gameOver do
        render walls fighters turn (input.[0].Length, input.Length) |> ignore
        let fighter = fighters.[index]
        if fighter.health > 0 then
            let enemyKind = match fighter.kind with Elf -> Goblin | _ -> Elf
            let enemies = 
                fighters 
                |> Array.mapi (fun i f -> i, f) 
                |> Array.filter (fun (_, e) -> e.kind = enemyKind && e.health > 0)
                |> Array.sortBy (fun (_, e) -> e.health)

            if enemies.Length = 0 then
                gameOver <- true
            else
                let targets = 
                    enemies 
                    |> Array.map (findPath fighter fighters walls)
                    |> Array.choose id
                let adjacent = 
                    targets 
                    |> Array.filter (fun (p, _, _) -> List.length p = 1)
                    |> Array.sortBy (fun (_, e, _) -> e.health)
                    |> Array.tryHead
                match adjacent with
                | Some (_, e, i) ->
                    fighters.[i] <- { e with health = e.health - fighter.attack }
                | None ->
                    let target = 
                        targets 
                        |> Array.sortBy (fun (p, _, _) -> List.length p, snd p.[0], fst p.[0])
                        |> Array.tryHead
                    match target with
                    | Some ([_], e, i) ->
                        fighters.[i] <- { e with health = e.health - fighter.attack }
                    | Some ([x, y; _], e, i) ->
                        fighters.[index] <- { fighter with x = x; y = y }
                        fighters.[i] <- { e with health = e.health - fighter.attack }
                    | Some ((x, y)::_, _, _) ->
                        fighters.[index] <- { fighter with x = x; y = y }
                    | _ -> ()

        index <- index + 1
        if index = fighters.Length && not gameOver then
            index <- 0
            turn <- turn + 1
            fighters <- fighters |> Array.sortBy (fun f -> f.y, f.x)

    let finalHealth = fighters |> Seq.filter (fun f -> f.health > 0) |> Seq.sumBy (fun f -> f.health)
    printfn "part 1: turn %i health %i score %i" turn finalHealth (turn * finalHealth)

    0
