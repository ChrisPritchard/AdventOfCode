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
    if n = 0 then x, y
    else
        let nx, ny = 
            match d with
            | 0 -> x + 1, y
            | 1 -> x, y + 1
            | 2 -> x - 1, y
            | _ -> x, y - 1
        let nx, ny = checkForWrap map nx ny d
        if map[ny][nx] = '#' then x, y
        else march map checkForWrap nx ny (n - 1) d

let rec crawler map checkForWrap x y d =
    function
    | [] -> ((y + 1) * 1000) + ((x + 1) * 4) + d
    | (Move n)::rem ->
        let x, y = march map checkForWrap x y n d
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
            | 0 -> (findOpen map[ny]), ny
            | 1 -> nx, (findOpen (col nx))
            | 2 -> (findOpenBack map[ny]), ny
            | _ -> nx, (findOpenBack (col nx))
        else nx, ny
    
    let x = Seq.findIndex ((=) '.') map[0]
    crawler map checkForWrap x 0 0 instructions

let part2() =
    let input = readEmbedded "day22"

    let map = input[0..input.Length - 3]
    let instructions = parse input[input.Length - 1]

    // for part 2, when exiting the board (wrapping), move to the right 'face'
    // this involves matching each 50x50 zone to six faces, and transforming coords based on the arrangement
    // transforming coords might drastically change x and y values, as well as change the direction

    // board in the input data looks like:
    //  ##      01      5
    //  #       2      301  
    // ##      34       2
    // #       5    
    // there is a centre 50x150 column, where vertical wraps work fine
    // top 100x50 jumps to second-bottom 100x50 horizontally, flipping direction 
    // bottom of right on top 100x50 wraps to right of second down 50x50, change from down to left and switching x/y
        
    // create 'portals', running along the edge of each unmatched section. these would be maps to values in the target edge
    // again there would be twelve of them. possibly creating maps of coord to coord would be easier than a transform function?
    // the advantage would be no need to determine face or anything - just check nx, ny in the portal map

    // for a given out-of-bounds x,y -> face x,y and direction
    let portals = Dictionary<int * int, int * int * int>()

    // all side coordinates, just out of bounds
    let side0left = Array.init 50 (fun y -> 49, y)
    let side0top = Array.init 50 (fun x -> 50 + x, -1)
    let side1top = Array.init 50 (fun x -> 100 + x, -1)
    let side1right = Array.init 50 (fun y -> 150, y)
    let side1bottom = Array.init 50 (fun x -> 100 + x, 50)
    let side2left = Array.init 50 (fun y -> 49, 50 + y)
    let side2right = Array.init 50 (fun y -> 100, 50 + y)
    let side3left = Array.init 50 (fun y -> -1, 100 + y)
    let side3top = Array.init 50 (fun x -> x, 99)
    let side4right = Array.init 50 (fun y -> 100, 100 + y)
    let side4bottom = Array.init 50 (fun x -> 50 + x, 150)
    let side5left = Array.init 50 (fun y -> -1, 150 + y)
    let side5right = Array.init 50 (fun y -> 50, 150 + y)
    let side5bottom = Array.init 50 (fun x -> x, 200)

    // side 0 (left) to side 3 (right)
    for y in 0..49 do
        let side0Left = 49, y
        let side3Right = 

    // adding left edge for side 0 (left to right on edge 3)
    let portalMap = 
        (Map.empty, [50..99])
        ||> List.fold (fun acc x -> 
            Map.add (x, -1) (0, 150 + (x - 50), 0) acc)

    // if only there was a way to model in 3d... almost like a rotating map where we can follow across the board
          
    let checkForWrap (map: string[]) nx ny d =
        if nx < 0 || ny < 0 || ny >= map.Length || nx >= map[ny].Length || map[ny][nx] = ' ' then
            let findOpen = Seq.findIndex ((<>) ' ')
            let findOpenBack = Seq.findIndexBack ((<>) ' ')
            let col x = [0..map.Length - 1] |> Seq.filter (fun y -> map[y].Length > x) |> Seq.map (fun y -> map[y][x]) |> asString
            match d with
            | 0 -> (findOpen map[ny]), ny
            | 1 -> nx, (findOpen (col nx))
            | 2 -> (findOpenBack map[ny]), ny
            | _ -> nx, (findOpenBack (col nx))
        else nx, ny
    
    let x = Seq.findIndex ((=) '.') map[0]
    crawler map checkForWrap x 0 0 instructions