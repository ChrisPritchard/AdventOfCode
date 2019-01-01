open System

let distance x1 y1 z1 x2 y2 z2 = abs (x1 - x2) + abs (y1 - y2) + abs (z1 - z2)

[<EntryPoint>]
let main _ =

    let input = IO.File.ReadAllLines "input.txt"
    let seperators = "pos=<,> r" |> Seq.toArray
    let parse = 
        Array.map System.Int64.Parse
        >> fun (n: int64 []) -> n.[0], n.[1], n.[2], n.[3]
    let split (s: string) = s.Split (seperators, StringSplitOptions.RemoveEmptyEntries)
    let nanobots = 
        input 
        |> Seq.map (split >> parse)
        |> Seq.toList

    let (sx, sy, sz, sr) = nanobots |> Seq.maxBy (fun (_,_,_,r) -> r)
    let inRange = 
        nanobots 
        |> List.filter (fun (x, y, z, _) -> distance x y z sx sy sz <= sr) 
        |> List.length 

    printfn "part 1: %i" inRange

    let minx, maxx, miny, maxy, minz, maxz = 
        nanobots
        |> List.fold (fun (minx, maxx, miny, maxy, minz, maxz) (x, y, z, _) -> 
            min minx x, max maxx x, min miny y, max maxy y, min minz z, max maxz z)
            (Int64.MaxValue, 0L, Int64.MaxValue, 0L, Int64.MaxValue, 0L)
    
    let rec findDistance dist =
        if dist > maxx - minx then dist
        else findDistance (dist * 2L)
    let startDistance = findDistance 1L

    let sphereCount dist maxSoFar = 
        [minx..dist..maxx] |> List.collect (fun x ->
        [miny..dist..maxy] |> List.collect (fun y ->
        [minz..dist..maxz] |> List.map (fun z -> x, y, z)))

    0
