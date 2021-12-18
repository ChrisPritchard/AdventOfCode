module Day17

open Common
open System

let processed = readEmbedded "day17"

let init () =
    processed |> Array.length |> ignore

let (tx1, tx2, ty1, ty2) =
    let p = split " target:x=..,y=" processed[0]
    int p[0], int p[1], int p[2], int p[3]

let within (x, y) =
    x >= tx1 && x <= tx2 && y >= ty1 && y <= ty2

let beyond (x, y) =
    x > tx2 || y < ty1

let physics (x, y) (vx, vy) =
    let (x, y) = x + vx, y + vy
    let vx = if vx > 0 then vx - 1 else 0
    let vy = vy - 1
    (x, y), (vx, vy)

let test (ivx, ivy) =
    let rec step p v my =
        let ((nx, ny), nv) = physics p v
        if beyond (nx, ny) then None
        else if within (nx, ny) then Some (max my ny)
        else
            step (nx, ny) nv (max my ny)
    step (0, 0) (ivx, ivy) 0

let part1 () =
    [|1..300|] |> Array.collect (fun x -> [|-100..100|] |> Array.map (fun y -> test (x, y))) |> Array.choose id |> Array.max

let part2 () =
    [|1..300|] |> Array.collect (fun x -> [|-100..100|] |> Array.map (fun y -> test (x, y))) |> Array.choose id |> Array.length