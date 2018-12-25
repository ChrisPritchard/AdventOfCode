open System
open System.IO

let compose input =
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
            | '(' ->
                let afterMap, newRemaining = plot map (x, y) remaining
                plot afterMap (x, y) newRemaining
            | ')' -> 
                map, remaining
            | '|' ->
                map, '('::remaining
            | _ -> 
                plot map (x, y) remaining
        | [] -> map, []
        
    input |> Seq.toList |> plot start (0, 0) |> fst

let render finalMap =
    let x, y, w, h = 
        finalMap |> Map.toList 
        |> List.fold (fun (x, y, w, h) ((ox, oy), _) -> 
            min x ox, min y oy, max w ox, max h oy) 
                (Int32.MaxValue, Int32.MaxValue, 0, 0)

    let rendered = 
        [y..h] |> List.map (fun line ->
        [x..w] |> List.map (fun i -> Map.tryFind (i, line) finalMap |> Option.defaultValue " ") |> String.concat "")

    File.WriteAllLines ("rendered.txt", rendered)
    //rendered |> List.iter (printfn "%s")

let breadthSearch map =

    let neighbours closed (x, y) =
        [-1,0;1,0;0,-1;0,1] 
        |> List.map (fun (dx, dy) -> 
            (x + dx, y + dy), (x + dx*2, y + dy*2))
        |> List.filter (fun (door, space) -> 
            not (Set.contains space closed) && 
            match Map.tryFind door map with
            | Some "|" | Some "-" -> true
            | _ -> false)
        |> List.map snd

    let expandPath (soFar, closed) path =
        let head = match path with | [] -> (0,0) | head::_ -> head
        let next = neighbours closed head
        List.fold (fun (s, c) n -> (n::path)::s, Set.add n c) (soFar, closed) next

    let rec expandPaths soFar closed = 
        let next, closed = soFar |> List.fold expandPath ([], closed)
        if next = [] then List.head soFar |> List.length 
        else expandPaths next closed

    expandPaths [[]] <| Set.empty.Add (0, 0)

    
    //  for each path find neighbours that are not closed, and generate new paths
    // trim all paths that cannot

[<EntryPoint>]
let main _ =
    
    //let input = "^ENNWSWW(NEWS|)SSSEEN(WNSE|)EE(SWEN|)NNN$"
    let input = File.ReadAllText "input.txt"

    let map = compose input
    let result = breadthSearch map
    printfn "part 1: %i" result

    0
