open System
open System.IO

let isCart = 
    function 
    | '^' | 'v'
    | '<' | '>' -> true
    | _ -> false

let underrail =
    function
    | '^' | 'v' -> '|'
    | '<' | '>' -> '-'
    | c -> c

let next ((x,y),c) =
    match c with
    | '^' -> x,y-1
    | 'v' -> x,y+1
    | '<' -> x-1,y
    | '>' -> x+1,y
    | _ -> failwith "invalid cart"

let checkchange t =
    function
    | '^' ->
        match t with 
        | '/' -> '>'
        | '\\' -> '<'
        | '|' -> '^'
        | _ -> 'X'
    | 'v' ->
        match t with 
        | '/' -> '<'
        | '\\' -> '>'
        | '|' -> 'v'
        | _ -> 'X'
    | '<' ->
        match t with 
        | '/' -> 'v'
        | '\\' -> '^'
        | '-' -> '<'
        | _ -> 'X'
    | '>' ->
        match t with 
        | '/' -> '^'
        | '\\' -> 'v'
        | '-' -> '>'
        | _ -> 'X'
    | _ -> failwith "invalid cart"

type Cart = {
    pos: int * int
    dir: char
    inter: int
}

[<EntryPoint>]
let main _ =
    
    let input = File.ReadAllLines "input.txt"
    let tiles =
        input |> Seq.mapi (fun y line -> 
        line |> Seq.mapi (fun x c -> (x, y), c))
        |> Seq.collect id |> Set.toList

    let rails = tiles |> List.map underrail |> Map.ofList
    let carts = tiles |> List.filter isCart |> List.map (fun (p,c) -> 
        { pos = p; dir = c; inter = 0 })
    
    let height, width = Seq.length input, Seq.length input.[0]
    let orderedPoints = [0..height] |> List.collect (fun y -> [0..width] |> List.map (fun x -> x, y))

    let rec firstCrash tiles = 
        let next =
            orderedPoints |> List.fold (fun state p -> 
                match List.tryFind (fun c -> c.pos = p) carts with
                | None -> state
                | Some c -> 

                else state)

    0
