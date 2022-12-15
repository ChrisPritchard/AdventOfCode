module Day14

open Common

let part1And2 () =
    let rock = 
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
    
    let rockCount = Set.count rock
    let bedRock = rock |> Seq.map snd |> Seq.max |> (+) 2

    let rec gravity blocked sand = 
        match sand with
        | None -> gravity blocked (Some (500, 0))
        | Some (x, y) ->
            let next = x, y + 1
            let next = if Set.contains next blocked then x - 1, y + 1 else next
            let next = if Set.contains next blocked then x + 1, y + 1 else next
            if Set.contains next blocked then gravity (Set.add (x, y) blocked) None
            else if snd next = bedRock then Set.count blocked - rockCount
            else gravity blocked (Some next)
    
    let part1 = gravity rock None

    let rec gravity2 blocked sand = 
        let isBlocked p = Set.contains p blocked || snd p = bedRock
        match sand with
        | None -> gravity2 blocked (Some (500, 0))
        | Some (x, y) ->
            let next = x, y + 1
            let next = if isBlocked next then x - 1, y + 1 else next
            let next = if isBlocked next then x + 1, y + 1 else next
            if isBlocked next then 
                if (x, y) = (500, 0) then 
                    (Set.count blocked - rockCount) + 1
                else gravity2 (Set.add (x, y) blocked) None
            else gravity2 blocked (Some next)

    let part2 = gravity2 rock None

    part1, part2



// used for debugging

// let renderMap blocked file = 
//     let mutable parsed = ""
//     for y in [0..600] do
//         for x in [400..600] do
//             parsed <- parsed + 
//                 if Set.contains (x, y) blocked then "X" else " "
//         parsed <- parsed + "\n"
//     System.IO.File.WriteAllText (file, parsed)