module Day19

open Common
open System.IO

let input = File.ReadAllText "./inputs/day19.txt" |> split ","

let inRayVM x y = 
    let io = Intcode.IO.create ()
    io.write (int64 x)
    io.write (int64 y)
    let mem = Intcode.memFrom input
    Intcode.run 0L 0L mem io |> ignore
    io.read () |> snd |> (=) 1L

let part1 () = 

    [0..49] |> List.sumBy (fun y ->
        [0..49] |> List.sumBy (fun x ->
            if inRayVM x y then 1 else 0))

let part2 () =

    let width, height = 50, 50

    let map = Array.init height (fun y ->
        Array.init width (fun x ->
            if inRayVM x y then 1 else 0))
            
    let max = 
        float (width-1) /
        ([0..height-1] |> List.find (fun y -> map.[y].[width-1] = 1) |> float)
    let min = 
        float (width-1) /
        ([height-1..(-1)..0] |> List.find (fun y -> map.[y].[width-1] = 1) |> float)

    let inRayApprox x y = 
        let pos = float x / float y
        let result = pos <= max && pos >= min
        result

    let approxX, approxY =
        [100..50000] 
        |> List.pick (fun y ->
            [100..50000] 
            |> List.tryFind (fun x ->
                inRayApprox x y && 
                inRayApprox (x + 99) y &&
                inRayApprox x (y + 99) &&
                inRayApprox (x + 99) (y + 99))
            |> Option.map (fun x -> x, y))

    let actualX, actualY =
        [approxY - 50..approxY] 
        |> List.pick (fun y ->
            [approxX - 50..approxX] 
            |> List.tryFind (fun x ->
                inRayVM x y && 
                inRayVM (x + 99) y &&
                inRayVM x (y + 99) &&
                inRayVM (x + 99) (y + 99))
            |> Option.map (fun x -> x, y))

    actualX * 10000 + actualY