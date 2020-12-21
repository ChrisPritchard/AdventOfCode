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

let arranged connections =
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
    let (n, g, edges) = connections.[0]
    let start = [ (0,0), g ] |> Map.ofList
    let acc = Set.empty.Add n
    let queue = edges |> Array.map (fun edge -> edge, (0,0), g) |> List.ofArray
    let rec mapper map acc queue =
        match queue with
        | [] -> map
        | (edge, (x, y), grid)::rem ->
            let (other, edges) = index.[edge]
            let (other, (dx, dy)) = connect grid other
            let nx, ny = x + dx, y + dy
            let map = Map.add (nx, ny) other map
            let acc = Set.add edge acc
            let newQueue = 
                edges 
                |> Array.filter (fun n -> not (Set.contains n acc)) 
                |> Array.map (fun n -> n, (nx, ny), other)
                |> List.ofArray
            let queue = List.append rem newQueue
            mapper map acc queue
    let result = mapper start acc queue
    
    let minx, miny, maxx, maxy =
        ((0, 0, 0, 0), result)
        ||> Map.fold (fun (minx, miny, maxx, maxy) (ox, oy) _ -> min minx ox, min miny oy, max maxx ox, max maxy oy)
    
    let final = Array.init ((maxy - miny + 1) * (dim - 2)) (fun _ -> Array.create ((maxx - minx + 1) * (dim - 2)) ' ')
    for y in [miny..maxy] do
        for x in [minx..maxx] do
            let grid = result.[x, y]
            for i in [1..8] do
                for j in [1..8] do
                    let fy = (y - miny)*8 + (i-1)
                    let fx = (x - minx)*8 + (j-1)
                    final.[fy].[fx] <- grid.[i].[j]
    final

let taniwhas map = 
    //                    # 
    //  #    ##    ##    ###
    //   #  #  #  #  #  #   
    let positions = 
        [|0,0; 1,1; 4,1; 5,0; 6,0; 7,1; 10,1; 11,0; 12,0; 13,1; 16,1; 17,0; 18,0; 19,0; 18,-1|]
    let isTaniwha map (x, y) =
        positions 
        |> Array.map (fun (dx, dy) -> x + dx, y + dy)
        |> Array.forall (fun (ox, oy) ->
            oy >= 0 && oy < Array.length map
            && ox >= 0 && ox < Array.length map.[0]
            && map.[oy].[ox] = '#')
    let allPoints map =
        [|0..Array.length map - 1|]
        |> Array.collect (fun y -> 
            [|0..Array.length map.[0] - 1|] 
            |> Array.map (fun x -> x, y))
    let taniwhaCount map =
        allPoints map |> Array.filter (isTaniwha map) |> Array.length
    let a1 = rotate map
    let a2 = rotate a1
    let a3 = rotate a2
    let b0 = flip map
    let b1 = rotate b0
    let b2 = rotate b1
    let b3 = rotate b2
    let g, t = 
        [map; a1; a2; a3; b0; b1; b2; b3]
        |> List.pick (fun g -> 
            let t = taniwhaCount g
            if t > 0 then Some (g, t) else None)
    allPoints g 
    |> Array.filter (fun (x, y) -> g.[y].[x] = '#')
    |> Array.length
    |> fun c -> c - (t * 15)

let part2 () =
    let connections = connections ()
    let map = arranged connections
    taniwhas map
