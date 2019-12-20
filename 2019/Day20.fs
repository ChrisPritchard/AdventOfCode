module Day20

open Common
open System
open System.IO

let input = File.ReadAllLines "./inputs/day20.txt"

let map = input |> Array.map (fun line -> line.ToCharArray ())

let valid x y = x >= 0 && y >= 0 && y < map.Length && x < map.[y].Length
let portal x y =
    [
        [x, y - 1; x, y - 2]
        [x, y + 1; x, y + 2]
        [x - 1, y; x - 2, y]
        [x + 1, y; x + 2, y]
    ]
    |> List.tryPick (fun points ->
        if not (List.forall (fun (x, y) -> valid x y && Char.IsUpper map.[y].[x]) points) then
            None
        else
            List.map (fun (x, y) -> map.[y].[x]) points |> asString |> Some)

let portals = 
    seq {
        for y = 0 to map.Length - 1 do
            for x = 0 to map.[y].Length - 1 do
                match portal x y with
                | Some key -> yield key, (x, y)
                | None -> ()
    } |> Map.ofSeq
let havePortals = 
    portals 
    |> Map.toSeq 
    |> Seq.map (fun o -> snd o, fst o) 
    |> Map.ofSeq

let adjacent x y =
    [
        x - 1, y; x + 1, y; x, y - 1; x, y + 1
    ] |> Seq.filter (fun (x, y) -> valid x y)

let part1 () =

    let start = portals.["AA"]
    let isGoal (x, y) = 
        portals.["ZZ"] = (x, y)
    let edges (x, y) =
        let normal = 
            adjacent x y |> Seq.filter (fun (x, y) -> map.[y].[x] = '.')
        if not (Map.containsKey (x, y) havePortals) then
            normal
        else 
            let portal = havePortals.[x, y]
            let exit = portal |> Seq.rev |> asString
            if Map.containsKey exit portals then
                Seq.append normal [portals.[exit]]
            else
                normal
        
    let path = BFS.run isGoal edges start |> Option.defaultValue []
    path.Length - 1

let part2 () =

    0