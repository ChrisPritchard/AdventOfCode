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

    let expandPath (soFar, closed, club1000) path =
        let head = match path with | [] -> (0,0) | head::_ -> head
        let next = neighbours closed head
        let newPaths, newClosed = List.fold (fun (s, c) n -> (n::path)::s, Set.add n c) (soFar, closed) next
        let nextClub1000 = club1000 + (if List.length path >= 999 then List.length next else 0)
        newPaths, newClosed, nextClub1000

    let rec expandPaths club1000 soFar closed = 
        let next, closed, club1000 = soFar |> List.fold expandPath ([], closed, club1000)
        if next = [] then List.head soFar |> List.length, club1000
        else expandPaths club1000 next closed

    expandPaths 0 [[]] <| Set.empty.Add (0, 0)

[<EntryPoint>]
let main _ =
    
    //let input = "^ENNWSWW(NEWS|)SSSEEN(WNSE|)EE(SWEN|)NNN$"
    let input = File.ReadAllText "input.txt"

    let map = compose input
    let result, club1000 = breadthSearch map
    printfn "part 1: %i" result
    printfn "part 2: %i" club1000

    0
