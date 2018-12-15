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
        [0..size-1] |> List.map (fun dy -> grid.[x + dx, y + dy])) |> List.sum

    let part1 =
        [1..298] |> List.collect (fun x -> 
        [1..298] |> List.map (fun y -> (x, y), squareVal 3 x y))
        |> List.maxBy snd
    
    printfn "part1: %i,%i" <|| fst part1

    let sums = range |> List.fold (fun (sums : Map<int * int, int>) (x, y) ->
        let sum = 
            grid.[x, y]
            + (Map.tryFind (x, y - 1) sums |> Option.defaultValue 0)
            + (Map.tryFind (x - 1, y) sums |> Option.defaultValue 0)
            - (Map.tryFind (x - 1, y - 1) sums |> Option.defaultValue 0)
        Map.add (x, y) sum sums) (Map.empty<int * int, int> |> Map.add (0, 0) 0)
    
    let part2 = 
        range 
        |> List.collect (fun (x, y) -> [0..300] |> List.map (fun s ->
            let total = 
                sums.[x, y]
                 - (Map.tryFind (x, y - s) sums |> Option.defaultValue 0)
                 - (Map.tryFind (x - s, y) sums |> Option.defaultValue 0)
                 + (Map.tryFind (x - s, y - s) sums |> Option.defaultValue 0)
            (total, x, y, s)))
        |> List.maxBy (fun (total,_,_,_) -> total)
        |> fun (_, x, y, s) -> x, y, s

    printfn "part2: %i,%i,%i" <||| part2

    0