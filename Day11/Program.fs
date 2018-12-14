open System

let power sn x y =
    let rackId = x + 10
    let power = ((rackId * y) + sn) * rackId
    let hundreds = power / 100 % 10
    hundreds - 5

[<EntryPoint>]
let main _ =
    let input = 8772
    let grid = 
        [1..300] |> List.collect (fun x -> 
        [1..300] |> List.map (fun y -> 
            (x, y), power input x y))
            |> Map.ofList

    let part1 =
        [1..298] |> List.collect (fun x -> 
        [1..298] |> List.map (fun y -> 
            (x, y), [0..2] |> List.collect (fun dx ->
            [0..2] |> List.map (fun dy -> grid.[x + dx, y + dy]))
            |> List.sum))
        |> List.maxBy snd
        
    printfn "part1: %i, %i" <|| fst part1

    0
