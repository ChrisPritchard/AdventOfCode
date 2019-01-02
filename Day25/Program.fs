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

    let rec findLinked set coords =
        let newLinks = coords |> List.except set |> List.filter (fun f -> List.exists (fun c -> distance c f <= 3) set)
        if newLinks = [] then
            set
        else
            findLinked (set @ newLinks) coords

    let folder coords soFar coord = 
        if List.exists (fun set -> Set.contains coord set) soFar then soFar
        else
            let newSet = findLinked [coord] coords |> Set.ofList
            newSet::soFar

    let constellations = coords |> List.fold (folder coords) []

    printfn "part 1: %i" <| List.length constellations

    0
