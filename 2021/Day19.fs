module Day19

open Common
open System

// note: my solution for day 19 takes about six-eight minutes to run :(

let processed = readEmbedded "day19"

let init () =
    processed |> Array.length |> ignore

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

let allRotations (scanner: (int * int * int)[]) =
    transforms |> Array.map (fun m -> scanner |> Array.map (fun p -> map p m))

let add (x1, y1, z1) (x2, y2, z2) =
    x1 + x2, y1 + y2, z1 + z2

let sub (x1, y1, z1) (x2, y2, z2) =
    x1 - x2, y1 - y2, z1 - z2

let allDists scanner =
    scanner |> Array.map (fun p -> p, scanner |> Array.map (fun o -> sub p o) |> Set.ofArray)

let overlap withPoints scanner =
    let baseOffsets = allDists withPoints
    allRotations scanner
    |> Array.tryPick (fun rot ->
        let otherOffsets = allDists rot
        let result = 
            baseOffsets 
            |> Array.choose (fun (p, o) -> 
                otherOffsets 
                |> Array.tryFind (fun (pp, oo) -> 
                    let matches = Set.intersect o oo |> Set.count
                    matches >= 10)
                |> Option.map (fun (pp, _) -> p, pp))
        if result.Length < 12 then None 
        else 
            let diff = 
                let (p1, p2) = result[0] in sub p1 p2
            let adjusted = rot |> Array.map (fun p -> add p diff)
            Some (adjusted, diff))

let mutable part1Result: (int * int * int)[] option = None

let part1 () =
    let rec matchAll acc others scannersAcc = 
        let (i, found, scanner) = 
            acc
            |> Array.pick (fun acc ->
                others
                |> Array.indexed 
                |> Array.tryPick (fun (i, scanner) -> 
                    match overlap acc scanner with 
                    | Some (newPoints, scanner) -> 
                        Some (i, newPoints, scanner)
                    | _ -> None))
        let scannersAcc = Array.append scannersAcc [|scanner|]
        if others.Length = 1 then
            Array.append acc [|found|] |> Array.concat |> Array.distinct, scannersAcc
        else
            matchAll (Array.append acc [|found|]) (Array.removeAt i others) scannersAcc
                
    let (points, allScanners) = matchAll [|scanners[0]|] scanners[1..] Array.empty
    part1Result <- Some allScanners
    Array.length points

let part2 () =

    let allScanners = 
        match part1Result with
        | Some p -> p
        | None ->
            part1 () |> ignore
            match part1Result with Some p -> p | _ -> failwith "something went wrong"
                
    let mh a b = 
        if a < 0 && b > 0 then abs a + b
        else if a > 0 && b < 0 then a + abs b
        else abs (abs a - abs b)
    allScanners 
    |> Array.collect (fun (x1, y1, z1) -> 
        allScanners |> Array.map (fun (x2, y2, z2) -> 
            mh x1 x2 + mh y1 y2 + mh z1 z2))
    |> Array.max |> int