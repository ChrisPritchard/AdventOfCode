module Day14

open Common

let part1 () =
    let blocked = 
        readEmbeddedRaw "day14"
        |> Seq.collect (fun line ->
            line
            |> split "-> ,"
            |> Array.map int 
            |> Array.chunkBySize 2 
            |> Array.windowed 2
            |> Seq.collect (fun pair ->
                let x1, y1 = pair[0][0], pair[0][1]
                let x2, y2 = pair[1][0], pair[1][1]
                [x1..(if x1 < x2 then 1 else -1)..x2]
                |> Seq.collect (fun x ->
                    [y1..(if y1 < y2 then 1 else -1)..y2] |> Seq.map (fun y -> x, y)
            )))
        |> Set.ofSeq
    let rock = Set.count blocked
    let overAbyss (x, y) = 
        [y..y+50] |> List.forall (fun y -> not (Set.contains (x, y) blocked))
    
    let rec gravity i blocked sand = 
        match sand with
        | None -> gravity (i + 1) blocked (Some (500, 0))
        | Some (x, y) ->
            let next = x, y + 1
            let next = if Set.contains next blocked then x - 1, y + 1 else next
            let next = if Set.contains next blocked then x + 1, y + 1 else next
            if Set.contains next blocked then gravity (i + 1) (Set.add (x, y) blocked) None
            else if overAbyss next then Set.count blocked - rock
            else gravity (i + 1) blocked (Some next)
    gravity 0 blocked None

let part2 () = 
    0
