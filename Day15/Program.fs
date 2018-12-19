open System
open System.IO

type Fighter = {
    x: int; y: int; 
    health: int
    attack: int
    kind: FighterKind
} 
with member __.Pos = __.x, __.y
and FighterKind = Goblin | Elf

let create x y kind = { x = x; y = y; health = 200; attack = 3; kind = kind }

let render walls fighters current turn (width, height) =
    //Console.Clear ()
    Console.CursorVisible <- false
    for y = 0 to height-1 do
        Console.CursorTop <- y   
        for x = 0 to width-1 do
            Console.CursorLeft <- x         
            let c =
                if Set.contains (x, y) walls then '#'
                else
                    match Array.tryFind (fun e -> e.health > 0 && e.x = x && e.y = y) fighters with
                    | Some f when Array.findIndex ((=) f) fighters = current -> 'X'
                    | Some f when f.kind = Goblin -> 'G'
                    | Some f when f.kind = Elf -> 'E'
                    | _ -> '.'
            Console.Write c
        Console.CursorLeft <- width + 1
        //Console.Write "                                    "

    // for row in fighters |> Seq.where (fun f -> f.health > 0) |> Seq.groupBy (fun f -> f.y) do
    //     for (i, f) in snd row |> Seq.sortBy (fun f -> f.x) |> Seq.mapi (fun i f -> i, f) do
    //     Console.CursorTop <- f.y
    //     Console.CursorLeft <- (width + 2) + i * 12
    //     Console.Write (sprintf "%s (%i, %i: %i)" (if f.kind = Goblin then "G" else "E") f.x f.y f.health)

    Console.CursorTop <- height + 1
    Console.CursorLeft <- 2
    Console.Write (sprintf "turn %i" turn)

    // System.Threading.Thread.Sleep 33
    Console.ReadKey true

let blockers walls (fighters : seq<Fighter>) start =
    fighters 
    |> Seq.filter (fun f -> f.health > 0)
    |> Seq.map (fun f -> f.Pos)
    |> Set.ofSeq
    |> Set.union walls
    |> Set.add start

let neighbours = [-1, 0; 1, 0; 0, -1; 0, 1]

let findStep (ox, oy) (goals : (int * int) list) blockers =
    let goalMap = Set.ofList goals

    let expander (paths, closed, found) path =
        let (x, y) = match path with [] -> (ox, oy) | head::_ -> head
        let next = neighbours |> List.map (fun (dx, dy) -> x + dx, y + dy)
        let results = next |> List.filter (fun p -> Set.contains p goalMap)
        if List.isEmpty results then
            let used = List.filter (fun p -> not <| Set.contains p closed) next
            List.map (fun p -> p::path) used @ paths, 
            Set.union closed <| Set.ofList used, 
            found
        else
            let resultPaths = List.map (fun p -> p::path |> List.rev) results
            paths, closed, resultPaths @ found

    let rec expand (pathsSoFar : (int * int) list list) closedSoFar =
        let (nextPaths, nextBlockers, results) =
            pathsSoFar 
            |> List.sortBy (fun p -> 
                match p.Length with 
                | 0 -> 0, 0
                | n -> snd p.[n-1], fst p.[n-1])
            |> List.fold expander ([], closedSoFar, [])
        if not <| List.isEmpty results then
            results
            |> List.groupBy List.head 
            |> List.map fst 
            |> List.sortBy (fun (x, y) -> y, x) 
            |> List.tryHead
        else if not <| List.isEmpty nextPaths then
            expand nextPaths nextBlockers
        else
            None

    expand [[]] blockers

let strikeTarget (x, y) enemyMap =
    neighbours 
    |> List.map (fun (nx, ny) -> Map.tryFind (x + nx, y + ny) enemyMap)
    |> List.choose id
    |> List.sortBy (fun (e, _) -> e.health, e.y, e.x)
    |> List.tryHead

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

    while not gameOver do
        //render walls fighters index turn (input.[0].Length, input.Length) |> ignore
        let fighter = fighters.[index]
        if fighter.health > 0 then

            let enemyKind = match fighter.kind with Elf -> Goblin | _ -> Elf
            let enemies = 
                fighters 
                |> Array.mapi (fun i f -> f, i) 
                |> Array.filter (fun (e, _) -> e.kind = enemyKind && e.health > 0)
                |> Array.toList

            if enemies.Length = 0 then
                gameOver <- true
            else
                let enemyMap = enemies |> Seq.map (fun (e, i) -> e.Pos, (e, i)) |> Map.ofSeq
                match strikeTarget fighter.Pos enemyMap with
                | Some (e, i) ->
                    fighters.[i] <- { e with health = e.health - fighter.attack }
                | None ->
                    let blockers = blockers walls fighters fighter.Pos
                    let squares = 
                        enemies
                        |> List.collect (fun (e, _) ->
                            neighbours 
                            |> List.map (fun (nx, ny) -> e.x + nx, e.y + ny)
                            |> List.filter (fun p -> Set.contains p blockers |> not))
                        |> List.distinct
                    
                    match findStep fighter.Pos squares blockers with
                    | Some (x, y) ->
                        fighters.[index] <- { fighter with x = x; y = y }
                        match strikeTarget (x, y) enemyMap with
                        | Some (e, i) ->
                            fighters.[i] <- { e with health = e.health - fighter.attack }
                        | _ -> ()
                    | _ -> ()

        index <- index + 1
        if index = fighters.Length && not gameOver then
            index <- 0
            turn <- turn + 1
            fighters <- fighters |> Array.sortBy (fun f -> f.y, f.x)

    let finalHealth = fighters |> Seq.filter (fun f -> f.health > 0) |> Seq.sumBy (fun f -> f.health)
    printfn "part 1: turn %i health %i score %i" turn finalHealth (turn * finalHealth)

    0