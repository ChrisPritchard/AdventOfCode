open System
open System.IO

let part1 claims =
    let (mx, my, mw, mh) =
        claims |> List.fold (fun (dx, dy, dw, dh) (ox, oy, ow, oh) ->
            min dx ox, min dy oy, max dw (ox + ow), max dh (oy + oh)) 
            (Int32.MaxValue, Int32.MaxValue, 0, 0)

    let contains (x, y) (ox, oy, ow, oh) =
        x >= ox && x < ox + ow
        && y >= oy && y < oy + oh

    [mx..mw] |> List.collect (fun x ->
    [my..mh] |> List.map (fun y -> 
        List.filter (contains (x, y)) claims
        |> fun l -> List.length l > 1))
    |> List.filter id
    |> List.length

[<EntryPoint>]
let main _ =
    
    let lines = File.ReadAllLines "input.txt"
    // let lines = [|
    //     "#1 @ 1,3: 4x4"
    //     "#2 @ 3,1: 4x4"
    //     "#3 @ 5,5: 2x2"
    // |]

    let splits = [|" ";":";",";"x"|]
    let splitOptions = StringSplitOptions.RemoveEmptyEntries

    let claims =
        lines
        |> Seq.map (fun line -> 
            line.Split(splits, splitOptions)
            |> Array.skip 2
            |> Array.map Int32.Parse
            |> Array.toList)
        |> Seq.map (fun l -> l.[0], l.[1], l.[2], l.[3])
        |> Seq.toList

    printfn "%i" <| part1 claims

    0
