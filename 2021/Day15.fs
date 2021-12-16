module Day15

open Common
open System

let processed = readEmbedded "day15" |> Array.map (fun s -> s.ToCharArray() |> Array.map (fun c -> int c - int '0'))

let init () =
    processed |> Array.length |> ignore

let height = processed.Length
let width = processed[0].Length

let part1 () =
    
    let ty = height - 1
    let tx = width - 1

    let gScores = [|0..ty|] |> Array.map (fun _ -> [|0..tx|] |> Array.map (fun _ -> Int32.MaxValue))
    let fScores = [|0..ty|] |> Array.map (fun _ -> [|0..tx|] |> Array.map (fun _ -> Int32.MaxValue))
    let h = 
        let h (y1, x1) (y2, x2) = ((float x2 - float x1)**2 + (float y2 - float y1)**2) |> sqrt |> int
        [|0..ty|] |> Array.map (fun y -> [|0..tx|] |> Array.map (fun x -> h (y, x) (ty, tx)))
    let n =
        let nextTo (y, x) = 
            [|0,-1;-1,0;1,0;0,1|] 
            |> Array.map (fun (dy, dx) -> y + dy, x + dx) 
            |> Array.filter (fun (y, x) -> y >= 0 && y <= ty && x >= 0 && x <= tx)
        [|0..ty|] |> Array.map (fun y -> [|0..tx|] |> Array.map (fun x -> nextTo (y, x)))

    gScores[0][0] <- 0
    fScores[0][0] <- h[0][0]

    let mutable openSet = [|0,0|]
    let mutable found = false
    while not found do
        let (cy, cx) = openSet[0]
        if (cy, cx) = (ty, tx) then
            found <- true
        else
            openSet <- openSet[1..]
            for (ny, nx) in n[cy][cx] do
                let tentative_gScore = gScores[cy][cx] + processed[ny][nx]
                if tentative_gScore < gScores[ny][nx] then
                    gScores[ny][nx] <- tentative_gScore
                    fScores[ny][nx] <- tentative_gScore + h[ny][nx]
                    if not (Array.contains (ny, nx) openSet) then
                        openSet <- Array.append [|ny, nx|] openSet |> Array.sortBy (fun (ny, nx) -> fScores[ny][nx])

    gScores[ty][tx]
    
let part2 () =

    let getRisk y x = 
        let raw = processed[y % height][x % width]
        let my = y / height
        let mx = x / width
        let n = (raw + my + mx)
        if n > 9 then n - 9
        else n

    let ty = height * 5 - 1
    let tx = width * 5 - 1

    let gScores = [|0..ty|] |> Array.map (fun _ -> [|0..tx|] |> Array.map (fun _ -> Int32.MaxValue))
    let fScores = [|0..ty|] |> Array.map (fun _ -> [|0..tx|] |> Array.map (fun _ -> Int32.MaxValue))
    let h = 
        let h (y1, x1) (y2, x2) = ((float x2 - float x1)**2 + (float y2 - float y1)**2) |> sqrt |> int
        [|0..ty|] |> Array.map (fun y -> [|0..tx|] |> Array.map (fun x -> h (y, x) (ty, tx)))
    let n =
        let nextTo (y, x) = 
            [|0,-1;-1,0;1,0;0,1|] 
            |> Array.map (fun (dy, dx) -> y + dy, x + dx) 
            |> Array.filter (fun (y, x) -> y >= 0 && y <= ty && x >= 0 && x <= tx)
        [|0..ty|] |> Array.map (fun y -> [|0..tx|] |> Array.map (fun x -> nextTo (y, x)))

    gScores[0][0] <- 0
    fScores[0][0] <- h[0][0]

    let mutable openSet = [|0,0|]
    let mutable found = false
    while not found do
        let (cy, cx) = openSet[0]
        if (cy, cx) = (ty, tx) then
            found <- true
        else
            openSet <- openSet[1..]
            for (ny, nx) in n[cy][cx] do
                let tentative_gScore = gScores[cy][cx] + getRisk ny nx
                if tentative_gScore < gScores[ny][nx] then
                    gScores[ny][nx] <- tentative_gScore
                    fScores[ny][nx] <- tentative_gScore + h[ny][nx]
                    if not (Array.contains (ny, nx) openSet) then
                        openSet <- Array.append [|ny, nx|] openSet |> Array.sortBy (fun (ny, nx) -> fScores[ny][nx])

    gScores[ty][tx]

    // let mutable edges = [|0, 0|]
    // let mutable dists = Map.empty |> Map.add (0,0) 0
    // let mutable visited = Set.empty
    
    // let mutable found = false
    // let target = height * 5 - 1, width * 5 - 1

    // let getRisk (y, x) = 
    //     let raw = processed[y % height][x % width]
    //     let my = y / height
    //     let mx = x / width
    //     let n = (raw + my + mx)
    //     if n > 9 then n - 9
    //     else n
    // let nextTo (y, x) = 
    //     [|0,-1;-1,0;1,0;0,1|] 
    //     |> Array.map (fun (dy, dx) -> y + dy, x + dx) 
    //     |> Array.filter (fun (y, x) -> y >= 0 && y < height * 5 && x >= 0 && x < width * 5)
    
    // while not found do
    //     let next = edges |> Array.minBy (fun e -> dists[e])
    //     if next = target then found <- true
    //     else
    //         visited <- Set.add next visited
    //         edges <- edges |> Array.except [|next|]

    //         let neighbours = nextTo next |> Array.filter (fun p -> not (Set.contains p visited))
    //         edges <- edges |> Array.append neighbours

    //         for n in neighbours do
    //             let alt = dists[next] + getRisk n
    //             match Map.tryFind n dists with
    //             | Some v when alt < v ->
    //                 dists <- Map.add n alt dists
    //             | None ->
    //                 dists <- Map.add n alt dists
    //             | _ -> ()

    // dists[target]
    
