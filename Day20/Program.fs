open System

[<EntryPoint>]
let main _ =
    
    let input = "WNE"

    let wall (x, y) map =
        [-1..1] |> List.collect (fun dx -> [-1..1] |> List.map (fun dy -> dx, dy))
        |> List.fold (fun map (dx, dy) -> 
            let x, y = x + dx, y + dy
            if Map.containsKey (x, y) map then map
            else Map.add (x, y) "#" map) map

    let start = [ (0, 0), "X" ] |> Map.ofList |> wall (0, 0)

    let plotter (map, (x, y)) next =
        let door, space =
            match next with
            | 'W' -> (x - 1, y), (x - 2, y)
            | 'N' -> (x, y - 1), (x, y - 2)
            | 'E' -> (x + 1, y), (x + 2, y)
            | 'S' -> (x, y + 1), (x, y + 2)
            | _ -> failwith "not implemented"
        map
        |> Map.add door (match next with 'W' | 'E' -> "|" | _ -> "-")
        |> Map.add space "."
        |> wall door
        |> wall space, space

    let finalMap, _ = input |> Seq.fold plotter (start, (0, 0))

    let x, y, w, h = 
        finalMap |> Map.toList 
        |> List.fold (fun (x, y, w, h) ((ox, oy), _) -> 
            min x ox, min y oy, max w ox, max h oy) 
                (Int32.MaxValue, Int32.MaxValue, 0, 0)

    let rendered = 
        [y..h] |> List.map (fun line ->
        [x..w] |> List.map (fun i -> finalMap.[i, line]) |> String.concat "")

    rendered |> List.iter (printfn "%s")

    0
