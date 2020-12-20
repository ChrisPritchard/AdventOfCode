module Day20

open Common
open System.IO

let input = File.ReadAllText "./inputs/day20.txt"

let processed () =
    input
    |> splitOn (newline + newline)
    |> Array.map (fun image ->
        let lines = image |> splitOn newline
        let n = lines.[0] |> split "Tile :" |> Array.head |> uint64
        let grid =
            lines
            |> Array.tail
            |> Array.map Seq.toArray
        let xlen, ylen = grid.[0].Length, grid.Length
        let edges = 
            [|
                [|0..xlen-1|] |> Array.map (fun x -> grid.[0].[x]) |> asString
                [|0..xlen-1|] |> Array.map (fun x -> grid.[ylen-1].[x]) |> asString
                [|0..ylen-1|] |> Array.map (fun y -> grid.[y].[0]) |> asString
                [|0..ylen-1|] |> Array.map (fun y -> grid.[y].[xlen-1]) |> asString
            |] |> Array.collect (fun s -> [|s;Seq.rev s |> asString|]) |> Set.ofArray
        n, grid, edges)

let part1 () =
    let images = processed ()
    let corners = 
        images
        |> Array.choose (fun (n, _, edges) ->
            let connected = 
                images
                |> Array.filter (fun (n2, _, edges2) ->
                    n2 <> n && Set.intersect edges edges2 |> Set.isEmpty |> not)
            if connected.Length = 2 then Some n else None)
    printfn "%A" corners
    corners |> Array.reduce (*)

let part2 () =
    0