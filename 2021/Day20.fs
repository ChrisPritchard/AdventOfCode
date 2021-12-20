module Day20

open Common
open System

let processed = readEmbedded "day20"

let init () =
    processed |> Array.length |> ignore

let algo = processed[0].ToCharArray()

let index first (x, y) m =
    let defaultChar = if not first && algo[0] = '#' then '1' else '0'
    [|x-1, y-1; x, y-1; x+1, y-1; x-1, y; x,y; x+1, y; x-1, y+1; x, y+1; x+1, y+1|]
    |> Array.map (fun p -> match Map.tryFind p m with Some '#' -> '1' | Some '.' -> '0' | _ -> defaultChar)
    |> asString
    |> fun s -> Convert.ToInt32(s, 2)

let enhance first m = 
    m |> Map.fold (fun c k _ -> Map.add k (algo[index first k m]) c) Map.empty

// used for debugging
// let render m = 
//     let mutable res = "\n"
//     let a = Map.toArray m
//     let xrange = a |> Array.map (fst >> fst) |> Array.sort
//     let yrange = a |> Array.map (fst >> snd) |> Array.sort
//     for y in [yrange[0]..yrange[yrange.Length - 1]] do
//         for x in [xrange[0]..xrange[xrange.Length - 1]] do
//             match Map.tryFind (x, y) m with 
//             | Some '#' -> res <- res + "#"
//             | _ -> res <- res + "."
//         res <- res + "\n"
//     res

let part1 () =
    
    let pic = 
        let rowLen = processed[1].Length
        let headerFooter = [|Array.create (rowLen + 4) '.' |> asString|]
        Array.concat [|headerFooter; headerFooter; processed[1..]; headerFooter; headerFooter|]
        |> Array.mapi (fun y s -> (".." + s + "..").ToCharArray() |> Array.mapi (fun x c -> (x, y), c))
        |> Array.collect id
        |> Map.ofArray
  
    pic |> enhance true |> enhance false |> Map.toArray |> Array.filter (fun (_,c) -> c = '#') |> Array.length

let part2 () =
    
    let pic = 
        let rowLen = processed[1].Length
        let headerFooter = Array.create 50 (Array.create (rowLen + 100) '.' |> asString)
        let prefixPostfix = Array.create 50 '.' |> asString
        Array.concat [|headerFooter; processed[1..]; headerFooter|]
        |> Array.mapi (fun y s -> (prefixPostfix + s + prefixPostfix).ToCharArray() |> Array.mapi (fun x c -> (x, y), c))
        |> Array.collect id
        |> Map.ofArray
  
    [1..50] |> List.fold (fun m i -> enhance (i % 2 = 1) m) pic
    |> Map.toArray |> Array.filter (fun (_,c) -> c = '#') |> Array.length