open System.IO

open Model
open Parsing

type Group
with 
    member __.effectivePower = __.units * __.attackStrength
    member __.effectiveDamageTo other =
        if List.contains __.attackType other.immunities then 0
        else if List.contains __.attackType other.weaknesses then __.effectivePower * 2
        else __.effectivePower
    member __.findTarget targets =
        targets 
        |> List.filter (fun group -> group.kind <> __.kind)
        |> List.sortByDescending (fun o -> __.effectiveDamageTo o, o.effectivePower, o.initiative)
        |> List.tryHead
        |> Option.bind (fun other -> if __.effectiveDamageTo other = 0 then None else Some other)

[<EntryPoint>]
let main _ =
    
    let input = File.ReadAllText "input.txt"
    let groups = parseInput input

    0
