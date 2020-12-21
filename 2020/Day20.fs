module Day20

open Common
open System.IO

let input = File.ReadAllText "./inputs/day20.txt"

let dim = 10 // assume 10*10 squares

let left (grid: char [][]) = [|0..dim-1|] |> Array.map (fun y -> grid.[y].[0]) |> asString
let right (grid: char [][]) = [|0..dim-1|] |> Array.map (fun y -> grid.[y].[dim-1]) |> asString
let up (grid: char [][]) = [|0..dim-1|] |> Array.map (fun x -> grid.[0].[x]) |> asString
let down (grid: char [][]) = [|0..dim-1|] |> Array.map (fun x -> grid.[dim-1].[x]) |> asString

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
                [| left grid; right grid; up grid; down grid |] 
                |> Array.collect (fun s -> [|s;Seq.rev s |> asString|]) 
                |> Set.ofArray
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

    let connect grid1 grid2 =
        let grid2vers = 
            [| 
                grid2; rotate grid2; rotate (rotate grid2); rotate (rotate (rotate grid2))
                flip grid2; rotate (flip grid2); rotate (rotate (flip grid2)); rotate (rotate (rotate (flip grid2)))
            |]
        [|
            up grid1, (0, -1)
            right grid1, (1, 0)
            down grid1, (0, 1)
            left grid1, (-1, 0)
        |] |> Array.pick (fun (edge, dir) ->
            let op = 
                if dir = (0, -1) then down
                elif dir = (1, 0) then left
                elif dir = (0, 1) then up
                else right
            grid2vers |> Array.tryFind (fun o -> op o = edge) |> Option.bind (fun g -> Some (g, dir)))
    
    let index = connections |> Array.map (fun (n, g, e) -> n, (g, e)) |> Map.ofArray
    0

    // next strat:
    // start with one point in place
    // for each edge: find rotation/flip to fit with existing
        // given a position, and a index
        // rotate/flip until it matches adjacent (only needs to match one?)

    // let _, grid, _ = Array.head connections
    // printfn "%A" grid
    // printfn "%A" <| rotate grid

    0

let part2 () =
    let connections = connections ()
    let map = arranged connections
    map