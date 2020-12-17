module Day17

open System.IO

let input = File.ReadAllLines "./inputs/day17.txt"

let state p m = match Map.tryFind p m with Some s -> s | _ -> '.'

let nextState p m adjacent = 
    let active = 
        adjacent p 
        |> Array.map (fun op -> state op m) 
        |> Array.filter (fun s -> s = '#') 
        |> Array.length
    match state p m with
    | '#' when active <> 2 && active <> 3 -> '.'
    | '.' when active = 3 -> '#'
    | s -> s

let iter m adjacent = 
    let positions =
        Map.toArray m
        |> Array.collect (fun (k, _) -> adjacent k |> Array.append [|k|])
        |> Array.distinct
    positions |> Array.map (fun p -> p, nextState p m adjacent) |> Map.ofArray

let part1 () =
    let processed =
        input 
        |> Array.mapi (fun y line -> 
            line |> Seq.mapi (fun x c -> (x, y, 0), c) |> Seq.toArray)
        |> Array.collect id
        |> Map.ofArray

    let cardinals = 
        [|-1..1|] |> Array.collect (fun dx -> 
            [|-1..1|] |> Array.collect (fun dy ->
                [|-1..1|] 
                |> Array.filter (fun dz -> not (dx = 0 && dy = 0 && dz = 0))
                |> Array.map (fun dz -> dx, dy, dz)))

    let adjacent (x, y, z) =
        cardinals
        |> Array.map (fun (dx, dy, dz) -> x + dx, y + dy, z + dz)

    let result =
        (processed, [0..5])
        ||> List.fold (fun m _ -> iter m adjacent)
    result |> Map.toArray |> Array.filter (fun (_, s) -> s = '#') |> Array.length

let part2 () =
    let processed =
        input 
        |> Array.mapi (fun y line -> 
            line |> Seq.mapi (fun x c -> (x, y, 0, 0), c) |> Seq.toArray)
        |> Array.collect id
        |> Map.ofArray

    let cardinals =
        [|-1..1|] |> Array.collect (fun dx -> 
            [|-1..1|] |> Array.collect (fun dy ->
                [|-1..1|] |> Array.collect (fun dz ->
                    [|-1..1|] 
                    |> Array.filter (fun dw -> not (dx = 0 && dy = 0 && dz = 0 && dw = 0)) 
                    |> Array.map (fun dw -> dx, dy, dz, dw))))

    let adjacent (x, y, z, w) =
        cardinals 
        |> Array.map (fun (dx, dy, dz, dw) -> x + dx, y + dy, z + dz, w + dw)

    let result =
        (processed, [0..5])
        ||> List.fold (fun m _ -> iter m adjacent)
    result |> Map.toArray |> Array.filter (fun (_, s) -> s = '#') |> Array.length