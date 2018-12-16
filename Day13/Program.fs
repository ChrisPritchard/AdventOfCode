open System
open System.IO

type Cart = {
    pos: int * int
    dir: char
    inter: int
}

let move cart =
    let x,y = cart.pos
    match cart.dir with
    | '^' -> { cart with pos = x,y-1 }
    | 'v' -> { cart with pos = x,y+1 }
    | '<' -> { cart with pos = x-1,y }
    | '>' -> { cart with pos = x+1,y }
    | _ -> failwith "invalid cart"

let advance cart tile =
    match cart.dir with
    | '^' ->
        match tile with 
        | '/' -> { cart with dir = '>' }
        | '\\' -> { cart with dir = '<' }
        | '|' -> { cart with dir = '^' }
        | '+' -> 
            match cart.inter % 3 with
            | 0 -> { cart with dir = '<'; inter = cart.inter + 1 }
            | 1 -> { cart with dir = '^'; inter = cart.inter + 1 }
            | _ -> { cart with dir = '>'; inter = cart.inter + 1 }
        | _ -> { cart with dir = 'X' }
    | 'v' ->
        match tile with 
        | '/' -> { cart with dir = '<' }
        | '\\' -> { cart with dir = '>' }
        | '|' -> { cart with dir = 'v' }
        | '+' -> 
            match cart.inter % 3 with
            | 0 -> { cart with dir = '>'; inter = cart.inter + 1 }
            | 1 -> { cart with dir = 'v'; inter = cart.inter + 1 }
            | _ -> { cart with dir = '<'; inter = cart.inter + 1 }
        | _ -> { cart with dir = 'X' }
    | '<' ->
        match tile with 
        | '/' -> { cart with dir = 'v' }
        | '\\' -> { cart with dir = '^' }
        | '-' -> { cart with dir = '<' }
        | '+' -> 
            match cart.inter % 3 with
            | 0 -> { cart with dir = 'v'; inter = cart.inter + 1 }
            | 1 -> { cart with dir = '<'; inter = cart.inter + 1 }
            | _ -> { cart with dir = '^'; inter = cart.inter + 1 }
        | _ -> { cart with dir = 'X' }
    | '>' ->
        match tile with 
        | '/' -> { cart with dir = '^' }
        | '\\' -> { cart with dir = 'v' }
        | '-' -> { cart with dir = '>' }
        | '+' -> 
            match cart.inter % 3 with
            | 0 -> { cart with dir = '^'; inter = cart.inter + 1 }
            | 1 -> { cart with dir = '>'; inter = cart.inter + 1 }
            | _ -> { cart with dir = 'v'; inter = cart.inter + 1 }
        | _ -> { cart with dir = 'X' }
    | _ -> failwith "invalid cart"

[<EntryPoint>]
let main _ =
    
    let input = File.ReadAllLines "input.txt"
    let tiles =
        input |> Seq.mapi (fun y line -> 
        line |> Seq.mapi (fun x c -> (x, y), c))
        |> Seq.collect id |> Seq.toList

    let rails = 
        tiles 
        |> List.map (function | '^' | 'v' -> '|' | '<' | '>' -> '-' | c -> c) 
        |> Map.ofList
    let carts = 
        tiles 
        |> List.filter (function | '^' | 'v' | '<' | '>' -> true | _ -> false) 
        |> List.map (fun (p,c) -> { pos = p; dir = c; inter = 0 })
    
    let height, width = Seq.length input, Seq.length input.[0]
    let orderedPoints = [0..height] |> List.collect (fun y -> [0..width] |> List.map (fun x -> x, y))

    0
