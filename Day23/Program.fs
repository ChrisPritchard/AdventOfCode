open System

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

    let distance x1 y1 z1 x2 y2 z2 = abs (x1 - x2) + abs (y1 - y2) + abs (z1 - z2)

    let (sx, sy, sz, sr) = nanobots |> Seq.maxBy (fun (_,_,_,r) -> r)
    let inRange = 
        nanobots 
        |> List.filter (fun (x, y, z, _) -> distance x y z sx sy sz <= sr) 
        |> List.length 

    printfn "part 1: %i" inRange

    0
