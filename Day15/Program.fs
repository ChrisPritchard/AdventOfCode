open System.IO
open Bandits

[<EntryPoint>]
let main _ =
    let input = File.ReadAllLines "input.txt"

    match runGame input 3 false with
    | Victory (_, turn, health) ->
        printfn "part1: turn %i health %i score %i" turn health (turn * health)
    | _ -> failwith "Unexpected loss"

    let part2 = 
        [4..200] |> List.fold (fun prevResult elfAttack ->
        match prevResult with
        | _, ElfDeath _ -> elfAttack, runGame input elfAttack true
        | _, _ -> prevResult) (0, ElfDeath 0)

    match part2 with
    | elfAttack, Victory (_, turn, health) ->
        printfn "part2: attack %i turn %i health %i score %i" elfAttack turn health (turn * health)
    | _ -> ()

    0