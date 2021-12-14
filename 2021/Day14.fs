module Day14

open Common
open System

let processed = 
    let a = readEmbedded "day14" 
    a[0].ToCharArray(), a[1..] |> Array.map (fun s -> split " ->" s |> fun b -> b[0].ToCharArray(), b[1][0]) |> Map.ofArray

let init () =
    snd processed |> Map.count |> ignore

let part1 () =
    let apply_step current =
        current
        |> Array.windowed 2
        |> Array.collect (fun p -> [|p[0]; (snd processed)[p]|])
        |> fun r -> Array.append r [|current[current.Length - 1]|]

    let res = [1..10] |> List.fold (fun c _ -> apply_step c) (fst processed) |> Array.groupBy id |> Array.map (fun (_, a) -> a.Length) |> Array.sort
    res[res.Length - 1] - res[0]
    
let part2 () =
    let apply_step current =
        // go through all rules. find matches, generates

    let start = (fst processed) |> Array.windowed 2 |> Array.map (fun a -> a, 1L) |> Map.ofArray
    let res = [1..10] |> List.fold (fun c _ -> apply_step c) start |> Map.toArray |> Array.sortBy snd
    snd res[res.Length - 1] - snd res[0]