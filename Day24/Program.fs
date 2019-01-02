open System.IO

open Model
open Parsing

let runTurn groups = 

    let attackOrder =
        groups
        |> List.sortByDescending (fun (o: Group) -> o.effectivePower, o.initiative)
        |> List.fold (fun (soFar, targets) group -> 
            let target = group.findTarget targets
            let remaining = 
                match target with 
                | Some t -> List.except [t] targets 
                | _ -> targets
            (group, target)::soFar, remaining) ([], groups)
        |> fst
    
    let unitsRemaining =
        attackOrder 
        |> List.sortByDescending (fun (a, _) -> a.initiative)
        |> List.fold (fun units (a, d) ->
            if Map.find a units <= 0 then units
            else
                let attacker = { a with units = Map.find a units }
                match d with
                | Some other ->
                    let unitsLost = attacker.effectiveDamageTo other / other.hitpoints
                    Map.add other (Map.find other units - unitsLost) units
                | _ -> units)
            (groups |> List.map (fun g -> g, g.units) |> Map.ofList)

    groups 
    |> List.map (fun g -> { g with units = Map.find g unitsRemaining }) 
    |> List.filter (fun g -> g.units > 0)

let rec runGame groups = 
    let afterTurn = runTurn groups
    let ofKind kind = List.tryFind (fun g -> g.kind = kind) afterTurn
    if ofKind ImmuneSystem = None then
        Infection, List.sumBy (fun g -> g.units) afterTurn
    else if ofKind Infection = None then
        ImmuneSystem, List.sumBy (fun g -> g.units) afterTurn
    else
        runGame afterTurn

let boost amount groups =
    groups |> List.map (fun g -> 
        if g.kind = Infection then g 
        else { g with attackStrength = g.attackStrength + amount })

[<EntryPoint>]
let main _ =
    
    let input = File.ReadAllText "input.txt"
    let groups = parseInput input

    let _, part1 = runGame groups
    printfn "part 1: %i" part1

    let winner, _ = runGame <| boost 100000 groups

    0
