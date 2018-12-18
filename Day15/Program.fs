open System
open System.IO
open AStar

type Fighter = {
    mutable x: int; mutable y: int; 
    mutable health: int
    attack: int
    kind: FighterKind
} 
with member __.Pos = __.x, __.y
and FighterKind = Goblin | Elf

let create x y kind = { x = x; y = y; health = 200; attack = 3; kind = kind }

let gScore (_, y1) (_, y2) = 
    if y1 = y2 then 0.9
    else 1.
let strikingDistance fighter enemy = 
    (fighter.x = enemy.x || fighter.y = enemy.y) &&
    abs (fighter.x - enemy.x) <= 1 && abs (fighter.y - enemy.y) <= 1

let render walls fighters turn = ()
    // Console.CursorVisible <- false
    // System.Threading.Thread.Sleep 200
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
    // for row in fighters |> List.groupBy (fun f -> f.y) do
    //     for (i, f) in snd row |> List.sortBy (fun f -> f.x) |> List.mapi (fun i f -> i, f) do
    //     Console.CursorTop <- f.y
    //     Console.CursorLeft <- 9 + i * 8
    //     Console.Write (sprintf "%s (%i)" (if f.kind = Goblin then "G" else "E") f.health)
    // Console.CursorTop <- 9
    // Console.CursorLeft <- 2
    // Console.Write (sprintf "turn %i" turn)
    // //Console.ReadKey true

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

    let neighbours others goal (x, y) =
        [(-1,0);(1,0);(0,-1);(0,1)] 
        |> Seq.map (fun (dx, dy) -> x + dx, y + dy)
        |> Seq.filter (fun p ->
            match 
                Set.contains p walls, 
                others |> List.tryFind (fun (f : Fighter) -> p <> goal && f.Pos = p) with
            | false, None -> true
            | _ -> false)

    let path fighter others goal = 
        AStar.search (fighter.x, fighter.y) goal { 
                neighbours = neighbours others goal
                gCost = gScore
                fCost = fun _ _ -> 0.
                maxIterations = None 
            }

    let advance others fighter =
        let others = others |> List.filter (fun o -> o.health > 0)
        let enemyKind = match fighter.kind with Elf -> Goblin | _ -> Elf
        let target = 
            others |> List.filter 
                (fun f -> f.kind = enemyKind && strikingDistance fighter f)
                |> List.sortBy (fun f -> f.health, f.y, f.x)
                |> List.tryHead
        match target with
        | Some e ->
            e.health <- e.health - fighter.attack
        | None ->
            let goal = 
                others 
                |> List.filter (fun f -> f.kind = enemyKind) 
                |> List.map (fun e -> 
                    match path fighter others e.Pos with
                    | Some p -> 
                        Some (p |> Seq.rev |> Seq.toList, e)
                    | None -> None)
                |> List.choose id
                |> List.sortBy (fun (p, _) -> 
                    match p with 
                    | (_::(x, y)::_) -> 
                        List.length p, y, x 
                    | _ -> 
                        List.length p, 0, 0)
                |> List.tryHead
            match goal with
            | Some ([_;(x, y);_], e) -> 
                fighter.x <- x
                fighter.y <- y
                e.health <- e.health - fighter.attack
            | Some (_::(x, y)::_, _) -> 
                fighter.x <- x
                fighter.y <- y
            | _ -> ()
        fighter

    let rec game walls fighters turn =
        let next =
            fighters 
            |> List.sortBy (fun f -> f.y, f.x)
            |> List.map (fun f -> if f.health > 0 then advance fighters f else f)
            |> List.filter (fun f -> f.health > 0)
        match next |> List.groupBy (fun f -> f.kind) |> List.length with
        | 1 -> 
            render walls next turn |> ignore
            turn, next |> List.sumBy (fun f -> f.health)
        | _ -> 
            //render walls next (turn + 1) |> ignore
            game walls next (turn + 1)

    let turn, health = game walls start 0
    printfn "part 1: turn %i health %i score %i" turn health (turn * health)
    0
