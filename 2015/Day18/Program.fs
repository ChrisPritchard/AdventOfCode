
open System.IO

[<EntryPoint>]
let main _ =

    let input = File.ReadAllLines "input.txt"

    let grid = 
        input 
        |> Seq.mapi (fun y line -> 
            line |> Seq.mapi (fun x c -> x, y, c)) 
        |> Seq.collect id
        |> Seq.fold (fun grid (x, y, c) -> 
            Map.add (x, y) (c = '#') grid) Map.empty

    let nextFor (x, y) grid =
        let neighbours =
            [-1..1] |> List.collect (fun dx -> [-1..1] |> List.map (fun dy -> x + dx, y + dy))
            |> List.except [x, y]
            |> List.map (fun p -> Map.tryFind p grid)
            |> List.choose id
            |> List.filter id
            |> List.length
        if grid.[x, y] then
            neighbours = 2 || neighbours = 3
        else 
            neighbours = 3

    let nextGrid grid =
        Map.toList grid
        |> List.map (fun (p, _) -> p, nextFor p grid)
        |> Map.ofList
    
    let after100 = [1..100] |> List.fold (fun grid _ -> nextGrid grid) grid
    let part1 = Map.toList after100 |> List.filter snd |> List.length
    printfn "part 1: %i" part1

    let after100 = [1..100] |> List.fold (fun grid _ -> 
        nextGrid grid
        |> Map.add (0, 0) true
        |> Map.add (0, 99) true
        |> Map.add (99, 0) true
        |> Map.add (99, 99) true) grid
    let part2 = Map.toList after100 |> List.filter snd |> List.length
    printfn "part 2: %i" part2

    0
