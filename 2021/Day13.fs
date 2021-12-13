module Day13

open Common
open System

let processed = 
    readEmbedded "day13" 
    |> Array.fold (fun (points, folds) (line: string) -> 
        if line.Contains "," then 
            let p = split "," line
            Set.add (int p[0], int p[1]) points, folds
        else
            let p = split " =" line
            points, Array.append folds [|p[2], int p[3]|]) 
        (Set.empty, Array.empty)

let init () =
    snd processed |> Array.length |> ignore

let part1 () =
    let (fold_axis, fold_point) = (snd processed)[0]
    if fold_axis = "y" then
        let (top, bottom) = fst processed |> Set.partition (fun (_, y) -> y > fold_point)
        let result = Set.union top (bottom |> Set.map (fun (x, y) -> x, fold_point - (y - fold_point)))
        Set.count result
    else
        let (left, right) = fst processed |> Set.partition (fun (x, _) -> x > fold_point)
        let result = Set.union left (right |> Set.map (fun (x, y) -> fold_point - (x - fold_point), y))
        Set.count result

let part2 () =
    0
    
