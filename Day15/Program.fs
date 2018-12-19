open System.IO

type Fighter = {
    x: int; y: int; 
    health: int
    kind: FighterKind
} 
with member __.Pos = __.x, __.y
and FighterKind = Goblin | Elf

let create x y kind = { x = x; y = y; health = 200; kind = kind }

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

let runGame walls start (attack : Map<FighterKind, int>) = 
    let mutable gameOver = false
    let mutable turn = 0
    let mutable index = 0;
    let mutable fighters = start |> List.sortBy (fun f -> f.y, f.x) |> List.toArray

    while not gameOver do
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
                    fighters.[i] <- { e with health = e.health - attack.[fighter.kind] }
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
                            fighters.[i] <- { e with health = e.health - attack.[fighter.kind] }
                        | _ -> ()
                    | _ -> ()

        index <- index + 1
        if index = fighters.Length && not gameOver then
            index <- 0
            turn <- turn + 1
            fighters <- fighters |> Array.sortBy (fun f -> f.y, f.x)

    turn, fighters        

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

    let part1Attack = [Goblin,3;Elf,3]|> Map.ofList
    let part1Turn, part1Fighters = runGame walls start part1Attack
    let finalHealth = part1Fighters |> Seq.filter (fun f -> f.health > 0) |> Seq.sumBy (fun f -> f.health)

    printfn "part 1: turn %i health %i score %i" part1Turn finalHealth (part1Turn * finalHealth)

    let elfCount (fighters : seq<Fighter>) = fighters |> Seq.sumBy (fun f -> if f.kind = Elf && f.health > 0 then 1 else 0)
    let startCount = elfCount start
    let mutable elfAttack = 4
    let mutable found = false
    while not found do
        printfn "trying elf attack %i..." elfAttack
        let part2Attack = [Goblin,3;Elf,elfAttack]|> Map.ofList
        let part2Turn, part2Fighters = runGame walls start part2Attack
        if elfCount part2Fighters = startCount then
            let finalHealth = part2Fighters |> Seq.filter (fun f -> f.health > 0) |> Seq.sumBy (fun f -> f.health)
            printfn "part 2: turn %i health %i score %i" part2Turn finalHealth (part2Turn * finalHealth)
            found <- true
        else
            elfAttack <- elfAttack + 1

    0