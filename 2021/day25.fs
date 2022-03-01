module Day25

open Common
open System

let (horizontal, vertical, xmax, ymax) = 
    let lines = readEmbedded "day25"
    let ymax = lines.Length
    let xmax = lines[0].Trim().Length
    let (h, v) = 
        lines
        |> Array.indexed
        |> Array.fold (fun (h, v) (y, line) ->
            line 
            |> Seq.indexed
            |> Seq.fold (fun (h, v) (x, c) ->
                match c with
                | '>' -> Set.add (x, y) h, v
                | 'v' -> h, Set.add (x, y) v
                | _ -> h, v) (h, v)) (Set.empty, Set.empty)
    h, v, xmax, ymax

let init () =
    (Set.count horizontal + Set.count vertical) |> ignore

let advance (dx, dy) h v =
    Set.map (fun (x, y) -> 
        let nx = x + dx
        let nx = if nx = xmax then 0 else nx
        let ny = y + dy
        let ny = if ny = ymax then 0 else ny
        if Set.contains (nx, ny) h || Set.contains (nx, ny) v then (x, y) else (nx, ny))

let part1 () =

    let rec step i h v =
        let dh = advance (1, 0) h v h
        let dv = advance (0, 1) dh v v
        if Set.isEmpty (Set.difference dh h) && Set.isEmpty (Set.difference dv v) then
            i
        else
            step (i + 1) dh dv

    step 1 horizontal vertical