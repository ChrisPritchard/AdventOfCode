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

let blockers walls (fighters : seq<Fighter>) start =
    fighters 
    |> Seq.filter (fun f -> f.health > 0)
    |> Seq.map (fun f -> f.Pos)
    |> Set.ofSeq
    |> Set.union walls
    |> Set.add start

let findPaths (fighter : Fighter) (enemies : seq<Fighter * int>) blockers =
    let enemyMap = enemies |> Seq.map (fun (e, i) -> e.Pos, (e, i)) |> Map.ofSeq

    let expander (paths, closed, found) path =
        let (x, y) = match path with [] -> fighter.Pos | head::_ -> head
        let next = 
            [-1, 0; 1, 0; 0, -1; 0, 1] 
            |> List.map (fun (dx, dy) -> x + dx, y + dy)
        let results = next |> List.filter (fun p -> Map.containsKey p enemyMap)
        if List.isEmpty results then
            let used = List.filter (fun p -> not <| Set.contains p closed) next
            List.map (fun p -> p::path) used @ paths, 
            Set.union closed <| Set.ofList used, 
            found
        else
            let resultPaths = List.map (fun p -> p::path) results
            paths, closed, resultPaths @ found

    let rec expand (pathsSoFar : (int * int) list list) closedSoFar =
        let (nextPaths, nextBlockers, results) =
            pathsSoFar |> List.fold expander ([], closedSoFar, [])
        if not <| List.isEmpty results then
            Some results
        else if not <| List.isEmpty nextPaths then
            expand nextPaths nextBlockers
        else
            None

    match expand [[]] blockers with
    | None -> []
    | Some results ->
        results |> List.map (fun p -> 
            let e, i = enemyMap.[List.head p]
            List.rev p, e, i)
        

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
        let fighter = fighters.[index]
        if fighter.health > 0 then
            let enemyKind = match fighter.kind with Elf -> Goblin | _ -> Elf
            let enemies = 
                fighters 
                |> Array.mapi (fun i f -> f, i) 
                |> Array.filter (fun (e, _) -> e.kind = enemyKind && e.health > 0)
                |> Array.sortBy (fun (e, _) -> e.health)

            if enemies.Length = 0 then
                gameOver <- true
            else
                let blockers = blockers walls fighters fighter.Pos
                let targets = findPaths fighter enemies blockers
                let adjacent = 
                    targets 
                    |> Seq.filter (fun (p, _, _) -> List.length p = 1)
                    |> Seq.sortBy (fun (_, e, _) -> e.health)
                    |> Seq.tryHead
                match adjacent with
                | Some (_, e, i) ->
                    fighters.[i] <- { e with health = e.health - fighter.attack }
                | None ->
                    let target = 
                        targets 
                        |> Seq.sortBy (fun (p, _, _) -> List.length p, snd p.[0], fst p.[0])
                        |> Seq.tryHead
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