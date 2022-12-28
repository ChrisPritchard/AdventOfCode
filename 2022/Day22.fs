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

    // all side coordinates, just out of bounds
    // there are three corner cases that need to be calculated seperately (affecting six sides)
    let side0left = Array.init 50 (fun y -> 49, y)
    let side0top = Array.init 50 (fun x -> 50 + x, -1)
    let side1top = Array.init 50 (fun x -> 100 + x, -1)
    let side1right = Array.init 50 (fun y -> 150, y)
    let side1bottom = Array.init 49 (fun x -> 101 + x, 50) // skipping corner 100,50
    let side2left = Array.init 49 (fun y -> 49, 50 + y) // skipping corner 49, 99
    let side2right = Array.init 49 (fun y -> 100, 51 + y) // skipping corner 100,50
    let side3left = Array.init 50 (fun y -> -1, 100 + y)
    let side3top = Array.init 49 (fun x -> x, 99) // skipping corner 49, 99
    let side4right = Array.init 50 (fun y -> 100, 100 + y)
    let side4bottom = Array.init 49 (fun x -> 51 + x, 150) // skipping corner 50, 150
    let side5left = Array.init 50 (fun y -> -1, 150 + y)
    let side5right = Array.init 49 (fun y -> 50, 151 + y) // skipping corner 50, 150
    let side5bottom = Array.init 50 (fun x -> x, 200)

    // for a given out-of-bounds x,y -> face x,y and direction
    let portals = Dictionary<int * int, int * int * int>()
    let withDirection d = 
        Array.map (fun (a, (x, y)) -> a, (x, y, d)) 
        >> fun a -> Array.ForEach (a, (fun (k, v) -> portals.Add(k, v)))

    //  ##      01      5
    //  #       2      301  
    // ##      34       2
    // #       5    

    Array.zip side0left (Array.map (fun (_, y) -> 0, y) side3left |> Array.rev) |> withDirection 0
    Array.zip side0top (Array.map (fun (_, y) -> 0, y) side5left) |> withDirection 0
    
    Array.zip side1top (Array.map (fun (x, _) -> x, 199) side5bottom |> Array.rev) |> withDirection 3
    Array.zip side1right (Array.map (fun (_, y) -> 99, y) side4right |> Array.rev) |> withDirection 2
    Array.zip side1bottom (Array.map (fun (_, y) -> 99, y) side2right) |> withDirection 2

    Array.zip side2left (Array.map (fun (x, _) -> x, 100) side3top) |> withDirection 1
    Array.zip side2right (Array.map (fun (x, _) -> x, 49) side1bottom) |> withDirection 3

    Array.zip side3left (Array.map (fun (_, y) -> 50, y) side0left |> Array.rev) |> withDirection 0
    Array.zip side3top (Array.map (fun (_, y) -> 59, y) side2left) |> withDirection 0

    Array.zip side4right (Array.map (fun (_, y) -> 149, y) side1right |> Array.rev) |> withDirection 2
    Array.zip side4bottom (Array.map (fun (_, y) -> 49, y) side5right) |> withDirection 2

    Array.zip side5left (Array.map (fun (x, _) -> x, 0) side0top) |> withDirection 1
    Array.zip side5right (Array.map (fun (x, _) -> x, 149) side4bottom) |> withDirection 3
    Array.zip side5bottom (Array.map (fun (x, _) -> x, 0) side1top |> Array.rev) |> withDirection 1
          
    let checkForWrap _ nx ny d =
        match nx, ny, d with
        | 49,99,2 -> 49,100,1 // side 2 left to side 3 top
        | 49,99,3 -> 50,100,0 // side 3 top to side 2 left
        | 50,150,1 -> 49,150,2 // side 4 bottom to side 5 right
        | 50,150,0 -> 50,149,1 // side 5 right to side 4 bottom
        | 100,50,1 -> 99,50,2 // side 1 bottom to side 2 right
        | 100,50,0 -> 100,49,3 // side 2 right to side 1 bottom
        | _ when portals.ContainsKey(nx, ny) -> portals[nx, ny]
        | _ -> nx, ny, d
    
    let x = Seq.findIndex ((=) '.') map[0]
    crawler map checkForWrap x 0 0 instructions