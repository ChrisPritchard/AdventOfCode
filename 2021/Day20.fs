module Day20

open Common
open System

let processed = readEmbedded "day20"

let init () =
    processed |> Array.length |> ignore

let part1 () =
    let algo = processed[0].ToCharArray()

    let pic = 
        processed[1..] 
        |> Array.mapi (fun y s -> s.ToCharArray() |> Array.mapi (fun x c -> (x, y), c))
        |> Array.collect id
        |> Map.ofArray

    let index (x, y) m =
        [|x-1, y-1; x, y-1; x+1, y+1; x-1, y; x,y; x+1, y; x-1, y+1; x, y+1; x+1, y+1|]
        |> Array.map (fun p -> match Map.tryFind p m with Some '#' -> '1' | _ -> '0')
        |> asString
        |> fun s -> Convert.ToInt32(s, 2)

    let enhance m = 
        m |> Map.fold (fun c k _ -> Map.add k (algo[index k m]) c) Map.empty

    let render m = 
        let mutable res = ""
        let a = Map.toArray m
        let xrange = a |> Array.map (fst >> fst) |> Array.sort
        let yrange = a |> Array.map (fst >> snd) |> Array.sort
        for y in yrange do
            for x in xrange do
                match Map.tryFind (x, y) m with 
                | Some '#' -> res <- res + "#"
                | _ -> res <- res + "."
            res <- res + "\n"
        res

    pic |> enhance |> enhance |> render

let part2 () =
    0