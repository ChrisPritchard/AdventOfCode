open System

[<EntryPoint>]
let main _ =
    
    let input = "^ENNWSWWSSSEENWNSEEENNN$"

    let wall (x, y) map =
        [-1..1] |> List.collect (fun dx -> [-1..1] |> List.map (fun dy -> dx, dy))
        |> List.fold (fun map (dx, dy) -> 
            let x, y = x + dx, y + dy
            if Map.containsKey (x, y) map then map
            else Map.add (x, y) "#" map) map

    let start = [ (0, 0), "X" ] |> Map.ofList |> wall (0, 0)

    let burrow (dx, dy) (sx, sy) =
        Map.add (dx, dy) (if dy = sy then "|" else "-")
        >> Map.add (sx, sy) "."
        >> wall (dx, dy)
        >> wall (sx, sy)

    let rec plot map (x, y) =
        function
        | next::remaining ->
            match next with
            | 'W' -> 
                let door, space = (x - 1, y), (x - 2, y)
                (burrow door space map, space, remaining) |||> plot
            | 'N' ->
                let door, space = (x, y - 1), (x, y - 2)
                (burrow door space map, space, remaining) |||> plot
            | 'E' ->
                let door, space = (x + 1, y), (x + 2, y)
                (burrow door space map, space, remaining) |||> plot
            | 'S' ->
                let door, space = (x, y + 1), (x, y + 2)
                (burrow door space map, space, remaining) |||> plot
            | _ -> 
                (map, (x, y), remaining) |||> plot
        | [] -> map
        
    let finalMap = input |> Seq.toList |> plot start (0, 0)

    let x, y, w, h = 
        finalMap |> Map.toList 
        |> List.fold (fun (x, y, w, h) ((ox, oy), _) -> 
            min x ox, min y oy, max w ox, max h oy) 
                (Int32.MaxValue, Int32.MaxValue, 0, 0)

    let rendered = 
        [y..h] |> List.map (fun line ->
        [x..w] |> List.map (fun i -> Map.tryFind (i, line) finalMap |> Option.defaultValue " ") |> String.concat "")

    rendered |> List.iter (printfn "%s")

    0
