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
    let across = blocked |> Seq.map fst |> Set.ofSeq
    let abyss x = not (Set.contains x across)
    
    let rec gravity blocked sand = 
        match sand with
        | None -> gravity blocked (Some (500, 0))
        | Some (x, y) ->
            let next = x, y + 1
            let next = if Set.contains next blocked then x - 1, y + 1 else next
            let next = if Set.contains next blocked then x + 1, y + 1 else next
            if Set.contains next blocked then gravity (Set.add (x, y) blocked) None
            else if abyss (fst next) then Set.count blocked - rock
            else gravity blocked (Some next)
    gravity blocked None

let part2 () = 
    0