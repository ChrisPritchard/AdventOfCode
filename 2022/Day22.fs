module Day22

open Common
open System
open System.Collections.Generic

type Instruction = 
    | Move of int
    | Right
    | Left

let parse (instructionText: string) =
    instructionText.Replace("R",",R,").Replace("L",",L,") 
    |> split "," 
    |> Array.map (fun part -> 
        if part = "R" then Right else if part = "L" then Left 
        else Move (Int32.Parse part))
    |> Array.toList

let rec march (map: string[]) checkForWrap x y n d = 
    if n = 0 then x, y, d
    else
        let nx, ny = 
            match d with
            | 0 -> x + 1, y
            | 1 -> x, y + 1
            | 2 -> x - 1, y
            | _ -> x, y - 1
        let nx, ny, d = checkForWrap map nx ny d
        if map[ny][nx] = '#' then x, y, d
        else march map checkForWrap nx ny (n - 1) d

let rec crawler map checkForWrap x y d =
    function
    | [] -> ((y + 1) * 1000) + ((x + 1) * 4) + d
    | (Move n)::rem ->
        let x, y, d = march map checkForWrap x y n d
        crawler map checkForWrap x y d rem
    | Right::rem ->
        let d = if d = 3 then 0 else d + 1
        crawler map checkForWrap x y d rem
    | Left::rem ->
        let d = if d = 0 then 3 else d - 1
        crawler map checkForWrap x y d rem

let part1() =
    let input = readEmbedded "day22"

    let map = input[0..input.Length - 3]
    let instructions = parse input[input.Length - 1]

    let checkForWrap (map: string[]) nx ny d =
        if nx < 0 || ny < 0 || ny >= map.Length || nx >= map[ny].Length || map[ny][nx] = ' ' then
            let findOpen = Seq.findIndex ((<>) ' ')
            let findOpenBack = Seq.findIndexBack ((<>) ' ')
            let col x = [0..map.Length - 1] |> Seq.filter (fun y -> map[y].Length > x) |> Seq.map (fun y -> map[y][x]) |> asString
            match d with
            | 0 -> (findOpen map[ny]), ny, d
            | 1 -> nx, (findOpen (col nx)), d
            | 2 -> (findOpenBack map[ny]), ny, d
            | _ -> nx, (findOpenBack (col nx)), d
        else nx, ny, d
    
    let x = Seq.findIndex ((=) '.') map[0]
    crawler map checkForWrap x 0 0 instructions

let part2() =
    let input = readEmbedded "day22"

    let map = input[0..input.Length - 3]
    let instructions = parse input[input.Length - 1]

    // board in the input data looks like:
    //  ##      01      5
    //  #       2      301  
    // ##      34       2
    // #       5    

    // there are 12 edges, e.g. above face 0 in the diagram. 
    // the problem can be solved by creating 'portals' along these edges, 50 to an edge
    // each portal is an x/y/d tuple, that maps to a new x/y/d tuple at the destination

    // e.g. 0-top is the point x=50-99,y=-1,d=3 (ds are 0=right,1=down,2=left,3=up)
    // these map to 5 left. y becomes 150 + (x - 50), x becomes 0, d becomes 0

    // mapping transforms is much, much easier if you create a paper model like i did

    let portals = Dictionary<int * int * int, int * int * int>()

    let edge (x1, y1, d1) (dx1, dy1) (x2, y2, d2) (dx2, dy2) =
        let edge1 = 
            if dx1 = 0 then
                [|y1..dy1..y1 + dy1 * 49|] |> Array.map (fun y -> x1, y, d1)
            else
                [|x1..dx1..x1 + dx1 * 49|] |> Array.map (fun x -> x, y1, d1)
        let edge2 = 
            if dx2 = 0 then
                [|y2..dy2..y2 + dy2 * 49|] |> Array.map (fun y -> x2, y, d2)
            else
                [|x2..dx2..x2 + dx2 * 49|] |> Array.map (fun x -> x, y2, d2)
        
        Array.ForEach ((Array.zip edge1 edge2), fun (e1, e2) -> portals.Add(e1, e2))
        Array.ForEach ((Array.zip edge2 edge1), fun (e2, e1) -> portals.Add(e2, e1))

    edge (50, -1, 3) (1, 0) (0, 150, 0) (0, 1) // 0 up to 5 left side going right
    edge (49, 0, 2) (0, 1) (0, 149, 0) (0, -1) // 0 left to 3 left side inverted going right
    edge (100, -1, 3) (1, 0) (0, 199, 3) (1, 0) // 1 up to 5 down side going up
    edge (150, 0, 0) (0, 1) (99, 149, 2) (0, -1) // 1 right to 4 right side inverted going left
    edge (100, 50, 1) (1, 0) (99, 50, 2) (0, 1) // 1 down to 2 right side inverted going left

    printfn "%A" portals