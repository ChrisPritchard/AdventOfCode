module Day19

open Common
open System

let processed = readEmbedded "day19"

let init () =
    processed |> Array.length |> ignore

let part1 () =
    let scanners = 
        processed 
        |> Array.fold (fun (acc, current) (line: string) ->
            match current with
            | None -> acc, Some Array.empty
            | Some l when line.StartsWith "---" -> Array.append acc [|l|], Some Array.empty
            | Some l -> 
                let points = line |> split "," |> Array.map float
                acc, Some (Array.append l [|points[0], points[1], points[2]|])) 
            (Array.empty, None)
        |> fun (acc, current) -> 
            match current with 
            | None -> acc 
            | Some c -> Array.append acc [|c|]
            
    let distance (x1, y1, z1) (x2, y2, z2) =
        sqrt ((x2 - x1)**2.0 + (y2 - y1)**2.0 + (z2 - z1)**2.0)

    let distances =
        scanners
        |> Array.map (fun scanner -> 
            scanner |> Array.map (fun point ->
                scanner 
                |> Array.filter (fun other -> other <> point)
                |> Array.map (fun other -> distance point other)
                |> Set.ofArray))
    
    let overlaps s1 s2 = 
        s1 |> Array.map (fun point1 ->
            s2 |> Array.map (fun point2 ->
                Set.union point1 point2 |> Set.count))

    overlaps distances[0] distances[1]
    

let part2 () =
    0