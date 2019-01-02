open System
open System.IO

[<EntryPoint>]
let main _ =
    
    let input = File.ReadAllLines "input.txt"
    let coords = 
        input 
        |> Seq.map (fun s -> s.Split [|','|] |> Seq.map Int32.Parse |> Seq.toList) 
        |> Seq.map (fun list -> list.[0], list.[1], list.[2], list.[3])
        |> Seq.toList

    let distance (x1,y1,z1,w1) (x2,y2,z2,w2) =
        abs (x2 - x1) + abs (y2 - y1) + abs (z2 - z1) + abs (w2 - w1)

    let folder coords soFar coord = 
        if List.tryFind (fun set -> Set.contains coord set) soFar <> None then soFar
        else
            let newSet = coords |> List.filter (fun c -> distance c coord <= 3) |> Set.ofList
            newSet::soFar

    let constellations = coords |> List.fold (folder coords) []

    printfn "part 1: %i" <| List.length constellations

    0
