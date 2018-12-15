open System

let power sn x y =
    let rackId = x + 10
    let power = ((rackId * y) + sn) * rackId
    let hundreds = power / 100 % 10
    hundreds - 5

[<EntryPoint>]
let main _ =
    let input = 18
    let range = [1..300] |> List.collect (fun y -> [1..300] |> List.map (fun x -> (x, y)))

    let grid = range |> List.map (fun (x, y) -> (x, y), power input x y) |> Map.ofList
    
    let squareVal size x y =
        [0..size-1] |> List.collect (fun dx ->
        [0..size-1] |> List.map (fun dy -> 
            Map.tryFind (x + dx, y + dy) grid |> Option.defaultValue 0)) 
        |> List.sum

    let part1 =
        [1..298] |> List.collect (fun x -> 
        [1..298] |> List.map (fun y -> (x, y), squareVal 3 x y))
        |> List.maxBy snd
    
    printfn "part1: %i,%i" <|| fst part1

    let part2 = 
        [1..300] |> List.collect (fun s ->
        [1..300] |> List.collect (fun x -> 
        [1..300] |> List.map (fun y -> (x, y, s, squareVal s x y))))
        |> List.maxBy (fun (_,_,_,t) -> t)
        |> fun (x,y,s,_) -> x,y,s

    printfn "part2: %i,%i,%i" <||| part2

    0