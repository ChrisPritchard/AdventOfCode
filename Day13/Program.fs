open System
open System.IO

let underrail =
    function
    | '^' | 'v' -> '|'
    | '<' | '>' -> '-'
    | c -> c

[<EntryPoint>]
let main _ =
    
    let input = File.ReadAllLines "input.txt"
    let tiles =
        input |> Seq.mapi (fun y line -> 
        line |> Seq.mapi (fun x c -> (x, y), c))
        |> Seq.collect id |> Seq.toList
    
    let carts = tiles |> List.filter (fun (_, c) -> 
        match c with '>' | '<' | '^' | 'v' -> true | _ -> false)
    let tileMap = tiles |> List.map (fun (p, c) -> p, underrail c) |> Map.ofList

    0
