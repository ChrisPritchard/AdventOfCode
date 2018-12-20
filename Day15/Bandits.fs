module Bandits

type GameResult = 
    | Victory of map: string array * turn: int * totalHealth: int
    | ElfDeath of turn: int

type Fighter = private {
    kind: FighterKind
    mutable pos: int * int
    mutable health: int
}
with 
    member __.X = match __.pos with (x, _) -> x
    member __.Y = match __.pos with (_, y) -> y
and FighterKind = private | Elf | Goblin

let private create pos kind = { kind = kind; pos = pos; health = 200 }

let processMap map =
    map 
    |> Seq.mapi (fun y line -> line |> Seq.mapi (fun x c -> (x, y, c)))
    |> Seq.collect id
    |> Seq.fold (fun (walls, fighters) (x, y, c) -> 
        match c with
        | '#' -> Set.add (x, y) walls, fighters
        | 'G' -> walls, (create (x, y) Goblin)::fighters
        | 'E' -> walls, (create (x, y) Elf)::fighters
        | _ -> walls, fighters) (Set.empty, [])

let composeMap walls fighters =
    let elves, goblins = 
        fighters
        |> List.fold (fun (elves, goblins) f ->
            if f.kind = Elf then Map.add f.pos f elves, goblins
            else elves, Map.add f.pos f goblins) (Map.empty, Map.empty)
    let width, height = 
        walls 
        |> Set.toList 
        |> List.fold (fun (width, height) (x, y) ->
            (if x > width then x else width), (if y > height then y else height)) (0, 0)
    [0..height] |> List.map (fun y -> 
        [0..width] |> List.map (fun x -> 
            if Set.contains (x, y) walls then "#"
            else if Map.containsKey (x, y) elves then "E"
            else if Map.containsKey (x, y) goblins then "G"
            else ".") |> String.concat "")
    |> List.toArray

let targetMap kind fighters =
    fighters 
    |> List.filter (fun f -> f.kind <> kind && f.health > 0) 
    |> List.map (fun f -> f.pos, f) 
    |> Map.ofList

let deltas = [0, -1; -1, 0; 1, 0; 0, 1]
let neighbours (x, y) = deltas |> List.map (fun (dx, dy) -> x + dx, y + dy)

let strikeAdjacent p enemyMap =
    neighbours p
    |> List.map (fun op -> Map.tryFind op enemyMap)
    |> List.choose id
    |> List.sortBy (fun e -> e.health, e.Y, e.X)
    |> List.tryHead

let findStep start enemyMap blockers =
    let goalMap = enemyMap |> Map.toList |> List.collect (fst >> neighbours) |> Set.ofList

    let expander (prev, closed, found) path =
        let next = 
            neighbours (match path with | p::_ -> p | _ -> start)
            |> List.filter (fun p -> not <| Set.contains p closed)
        let nextPaths = List.map (fun p -> p, p::path) next
        (List.map snd nextPaths) @ prev, 
        next |> Set.ofList |> Set.union closed,
        (List.filter (fun (p, _) -> Set.contains p goalMap) nextPaths) @ found

    let rec expand soFar closed = 
        let next, closed, found = soFar |> List.fold expander ([], closed, [])
        if List.isEmpty found then expand next closed
        else
            found 
            |> List.sortBy (fun ((px, py), path) -> py, px, snd path.[0], fst path.[0])
            |> List.tryHead
            |> Option.map (snd >> Seq.last)

    expand [[]] <| Set.add start blockers

let runFighterTurn (walls, fighters) elfAttack shouldFailOnElfDeath (prev, gameOver) fighter =
    if gameOver then
        fighter::prev, gameOver
    else
        let enemies = targetMap fighter.kind fighters            
        if Map.isEmpty enemies then
            fighter::prev, true
        else
            match strikeAdjacent fighter.pos enemies with
            | Some e ->
                e.health <- e.health - (if fighter.kind = Elf then elfAttack else 3)
                let gameOver = e.health < 1 && e.kind = Elf && shouldFailOnElfDeath
                fighter::prev, gameOver
            | None ->
                let blockers = fighters |> List.map (fun f -> f.pos) |> Set.ofList |> Set.union walls
                match findStep fighter.pos enemies blockers with
                | None -> fighter::prev, false
                | Some s ->
                    fighter.pos <- s
                    match strikeAdjacent fighter.pos enemies with
                    | Some e ->
                        e.health <- e.health - (if fighter.kind = Elf then elfAttack else 3)
                        let gameOver = e.health < 1 && e.kind = Elf && shouldFailOnElfDeath
                        fighter::prev, gameOver
                    | None ->
                        fighter::prev, false

let runTurn (walls, fighters) elfAttack shouldFailOnElfDeath =
    fighters 
    |> List.filter (fun f -> f.health > 0)
    |> List.sortBy (fun (f: Fighter) -> f.Y, f.X)
    |> List.fold (runFighterTurn (walls, fighters) elfAttack shouldFailOnElfDeath) ([], false)

let runGame startMap elfAttack shouldFailOnElfDeath =
    let walls, startFighters = processMap startMap

    let rec runTurns fighters lastTurnCount =
        let (newFighters, gameOver) = runTurn (walls, fighters) elfAttack shouldFailOnElfDeath
        if gameOver then 
            lastTurnCount, newFighters |> List.filter (fun f -> f.health > 0)
        else
            runTurns newFighters (lastTurnCount + 1)

    let turn, finalFighters = runTurns startFighters 0
    let finalMap = composeMap walls finalFighters
    Victory (finalMap, turn, finalFighters |> List.sumBy (fun f -> f.health))