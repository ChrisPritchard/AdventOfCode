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

let apply_rule current (fold_axis, fold_point) =
    if fold_axis = "y" then
        let (top, bottom) = current |> Set.partition (fun (_, y) -> y < fold_point)
        Set.union top (bottom |> Set.map (fun (x, y) -> x, fold_point - (y - fold_point)))
    else
        let (left, right) = current |> Set.partition (fun (x, _) -> x < fold_point)
        Set.union left (right |> Set.map (fun (x, y) -> fold_point - (x - fold_point), y))

let part1 () =
    apply_rule (fst processed) ((snd processed)[0]) |> Set.count

let part2 () =
    let rules = snd processed
    let result = rules |> Array.fold apply_rule (fst processed)    

    let (lines, width) = 
        rules 
        |> Array.fold (fun (l, w) (axis, value) -> 
            if axis = "y" && value < l then value, w 
            else if axis = "x" && value < w then l, value 
            else l, w) (999, 999)
    let readable =
        [0..lines] 
        |> List.map (fun y -> 
            [0..width] 
            |> List.map (fun x -> 
                if Set.contains (x, y) result then '#' else ' ') 
            |> asString) 
        |> String.concat "\n"
    "\n" + readable