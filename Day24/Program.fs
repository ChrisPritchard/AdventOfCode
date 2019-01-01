open System.IO

open Model
open Parsing

let runTurn groups = 
    let attackOrder =
        groups
        |> List.sortByDescending (fun (o: Group) -> o.effectivePower (), o.initiative)
        |> List.fold (fun (soFar, targets) group -> 
            let target = group.findTarget targets
            let remaining = 
                match target with 
                | Some t -> List.except [t] targets 
                | _ -> targets
            (group, target)::soFar, remaining) ([], groups)
        |> fst
    
    attackOrder 
    |> List.sortByDescending (fun (a, _) -> a.initiative)
    |> List.iter (fun (a, d) ->
        if a.units <= 0 then ()
        else
            match d with
            | Some other ->
                let unitsLost = a.effectiveDamageTo other / other.hitpoints
                other.units <- other.units - unitsLost
            | _ -> ())

    groups

let rec runGame groups = 
    let afterTurn = runTurn groups |> List.filter (fun g -> g.units > 0)
    let ofKind kind = List.tryFind (fun g -> g.kind = kind) afterTurn
    if ofKind ImmuneSystem = None || ofKind Infection = None then
        List.sumBy (fun g -> g.units) afterTurn
    else
        runGame afterTurn

[<EntryPoint>]
let main _ =
    
    let input = File.ReadAllText "input.txt"
    let groups = parseInput input

    let part1 = runGame groups
    printfn "part 1: %i" part1

    0
