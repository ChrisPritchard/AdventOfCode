module Day20

open Common
open System.IO

let input = File.ReadAllText "./inputs/day20.txt"

let dim = 10 // assume 10*10 squares

let connections () =
    let connections = 
        input
        |> splitOn (newline + newline)
        |> Array.map (fun image ->
            let lines = image |> splitOn newline
            let n = lines.[0] |> split "Tile :" |> Array.head |> uint64
            let grid =
                lines
                |> Array.tail
                |> Array.map Seq.toArray
            let edges = 
                [|
                    [|0..dim-1|] |> Array.map (fun x -> grid.[0].[x]) |> asString
                    [|0..dim-1|] |> Array.map (fun x -> grid.[dim-1].[x]) |> asString
                    [|0..dim-1|] |> Array.map (fun y -> grid.[y].[0]) |> asString
                    [|0..dim-1|] |> Array.map (fun y -> grid.[y].[dim-1]) |> asString
                |] |> Array.collect (fun s -> [|s;Seq.rev s |> asString|]) |> Set.ofArray
            n, grid, edges)
    connections
    |> Array.map (fun (n, grid, edges) ->
        let connected = 
            connections
            |> Array.choose (fun (n2, _, edges2) ->
                if n2 <> n && not (Set.intersect edges edges2 |> Set.isEmpty) then Some n2 else None)
        n, grid, connected)

let part1 () =
    connections ()
    |> Array.choose (fun (n, _, edges) ->
        if edges.Length = 2 then Some n else None)
    |> Array.reduce (*)

let arranged connections =
    let flip grid = 
        grid
        |> Array.map (fun (line: char []) ->
            [|0..dim-1|]
            |> Array.map (fun i -> line.[dim-1-i]))

    let rotate (grid: char [][]) =
        [|0..dim-1|]
        |> Array.map (fun y ->
            [|0..dim-1|] |> Array.map (fun x ->
                grid.[dim-1-x].[y]))
    
    // let _, grid, _ = Array.head connections
    // printfn "%A" grid
    // printfn "%A" <| rotate grid

    // create a map of x, y grid
    // start with first, at 0,0
    // for each edge, find relx,rely and orientation
    // if no edges match, then flip grid and try again
    0

let part2 () =
    let connections = connections ()
    let map = arranged connections
    map