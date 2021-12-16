module Day15

open Common
open System

let processed = readEmbedded "day15" |> Array.map (fun s -> s.ToCharArray() |> Array.map (fun c -> int c - int '0'))

let init () =
    processed |> Array.length |> ignore

let height = processed.Length
let width = processed[0].Length

type Heap(maxSize, comparer) =
    let data = Array.zeroCreate maxSize
    let mutable nextIndex = 0

    member this.Enqueue item =
        data[nextIndex] <- item

        let mutable ci = nextIndex
        while ci > 0 do
            let pi = (ci - 1) / 2
            if comparer data[ci] data[pi] >= 0 then ci <- -1 //break
            else
                let tmp = data[ci]
                data[ci] <- data[pi]
                data[pi] <- tmp
                ci <- pi

        nextIndex <- nextIndex + 1

    member this.Dequeue () =
        let mutable li = nextIndex - 1
        let frontItem = data[0]
        data[0] <- data[li]
        nextIndex <- nextIndex - 1

        li <- li - 1
        let mutable pi = 0

        let mutable finished = false
        while not finished do
            let mutable ci = pi * 2 + 1
            if ci > li then finished <- true
            else
                let rc = ci + 1
                if rc <= li && comparer data[rc] data[ci] < 0 then
                    ci <- rc
                if comparer data[pi] data[ci] <= 0 then finished <- true
                else
                    let tmp = data[pi]
                    data[pi] <- data[ci]
                    data[ci] <- tmp
                    pi <- ci

        frontItem

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

    let openSet = Heap(width * height, fun (x1, y1) (x2, y2) -> fScores[x1][y1] - fScores[x2][y2])
    openSet.Enqueue (0, 0)

    let mutable found = false
    while not found do
        let (cy, cx) = openSet.Dequeue()
        if (cy, cx) = (ty, tx) then
            found <- true
        else
            for (ny, nx) in n[cy][cx] do
                let tentative_gScore = gScores[cy][cx] + processed[ny][nx]
                if tentative_gScore < gScores[ny][nx] then
                    gScores[ny][nx] <- tentative_gScore
                    fScores[ny][nx] <- tentative_gScore + h[ny][nx]
                    openSet.Enqueue(ny, nx)

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

    let openSet = Heap(width * 5 * height * 5, fun (x1, y1) (x2, y2) -> fScores[x1][y1] - fScores[x2][y2])
    openSet.Enqueue (0, 0)

    let mutable found = false
    while not found do
        let (cy, cx) = openSet.Dequeue ()
        if (cy, cx) = (ty, tx) then
            found <- true
        else
            for (ny, nx) in n[cy][cx] do
                let tentative_gScore = gScores[cy][cx] + getRisk ny nx
                if tentative_gScore < gScores[ny][nx] then
                    gScores[ny][nx] <- tentative_gScore
                    fScores[ny][nx] <- tentative_gScore + h[ny][nx]
                    openSet.Enqueue(ny, nx)

    gScores[ty][tx]
