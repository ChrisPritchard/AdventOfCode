module Day19

open Common
open System

let processed = readEmbedded "day19"

let init () =
    processed |> Array.length |> ignore

let transforms = [|
        [|1, 0, 0; 0, 1, 0; 0, 0, 1|]
        [|1, 0, 0; 0, 0, -1; 0, 1, 0|]
        [|1, 0, 0; 0, -1, 0; 0, 0, -1|]
        [|1, 0, 0; 0, 0, 1; 0, -1, 0|]

        [|0, -1, 0; 1, 0, 0; 0, 0, 1|]
        [|0, 0, 1; 1, 0, 0; 0, 1, 0|]
        [|0, 1, 0; 1, 0, 0; 0, 0, -1|]
        [|0, 0, -1; 1, 0, 0; 0, -1, 0|]

        [|-1, 0, 0; 0, -1, 0; 0, 0, 1|]
        [|-1, 0, 0; 0, 0, -1; 0, -1, 0|]
        [|-1, 0, 0; 0, 1, 0; 0, 0, -1|]
        [|-1, 0, 0; 0, 0, 1; 0, 1, 0|]

        [|0, 1, 0; -1, 0, 0; 0, 0, 1|]
        [|0, 0, 1; -1, 0, 0; 0, -1, 0|]
        [|0, -1, 0; -1, 0, 0; 0, 0, -1|]
        [|0, 0, -1; -1, 0, 0; 0, 1, 0|]

        [|0, 0, -1; 0, 1, 0; 1, 0, 0|]
        [|0, 1, 0; 0, 0, 1; 1, 0, 0|]
        [|0, 0, 1; 0, -1, 0; 1, 0, 0|]
        [|0, -1, 0; 0, 0, -1; 1, 0, 0|]

        [|0, 0, -1; 0, -1, 0; -1, 0, 0|]
        [|0, -1, 0; 0, 0, 1; -1, 0, 0|]
        [|0, 0, 1; 0, 1, 0; -1, 0, 0|]
        [|0, 1, 0; 0, 0, -1; -1, 0, 0|]
    |]

let map (x, y, z) (m: (int * int * int)[]) =
    let (mx0, my0, mz0) = m[0]
    let (mx1, my1, mz1) = m[1]
    let (mx2, my2, mz2) = m[2]
    mx0 * x + my0 * y + mz0 * z, mx1 * x + my1 * y + mz1 * z, mx2 * x + my2 * y + mz2 * z

let scanners = 
    processed 
    |> Array.fold (fun (acc, current) (line: string) ->
        match current with
        | None -> acc, Some Array.empty
        | Some l when line.StartsWith "---" -> Array.append acc [|l|], Some Array.empty
        | Some l -> 
            let points = line |> split "," |> Array.map int
            acc, Some (Array.append l [|points[0], points[1], points[2]|])) 
        (Array.empty, None)
    |> fun (acc, current) -> 
        match current with 
        | None -> acc 
        | Some c -> Array.append acc [|c|]

let part1 () =
    let rotated = 
        transforms
        |> Array.map (fun m -> scanners[0] |> Array.map (fun p -> map p m))
    rotated

    // take scanner 0 as target
    // calculate change between each point
    // to test overlap 1
    // get all rotations of scanner 1
    // calculate change between each point on each rotation
    // for each change set, compare with change set on scanner 0
    // that is, each point in change set compared with each point in scanner 0
    // high union indicates same point

    // if same points found, calculate offset between
    // apply offset
    // 12 points at least should overlap, and scanner 2 can be determined

let part2 () =
    "not finished"