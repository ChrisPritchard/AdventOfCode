module Day22

open Common
open System

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
    //  ##      01
    //  #       2
    // ##      34
    // #       5
    // there is a centre 50x150 column, where vertical wraps work fine
    // top 100x50 jumps to second-bottom 100x50 horizontally, flipping direction 
    // bottom of right on top 100x50 wraps to right of second down 50x50, change from down to left and switching x/y

    let face x y = 
        if y < 50 then
            if x < 100 then 0 else 1
        else if y < 100 then 2
        else if y < 150 then
            if x < 50 then 3 else 4
        else 5
        
    // describe each face in its 3d orientation
    // from that, derive its adjacent sides?
    // and from *that*, determine the transforms?
    // is there an easier way?

    // possibly track orientation...

    // or just hardwire the edges. there are only 12, each which can be crossed by movement 
    // from a single direction on a given face. we could start with face 0, which has a left and top wrapping edge

    // face to direction to x/y/d transform
    let edges = [|
        0, [|
            2, fun (_, y) -> 0, 100 + (50 - y), 0 // left edge, low to high y, moving right on 3
            3, fun (x, _) -> 0, 150 + x, 0 // top edge, x becomes y, y becomes x, moving right on 5
        |]
        3, [|
            2, fun (_, y) -> 50, 50 - (150 - y), 0 // moving left off 3, to appear moving right on 0
            3, fun (x, _) -> 50, 50 + x, 0 // moving top off 3, to appear moving right on 2
        |]
    |]

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