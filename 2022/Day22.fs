module Day22

open Common
open System
open System.Collections.Generic

type Instruction = 
    | Move of int
    | Right
    | Left

let part1() =
    let input = readEmbedded "day22"

    let map = input[0..input.Length - 3]
    let instructions = 
        input[input.Length - 1].Replace("R",",R,").Replace("L",",L,") 
        |> split "," 
        |> Array.map (fun part -> 
            if part = "R" then Right else if part = "L" then Left 
            else Move (Int32.Parse part))
        |> Array.toList

    let checkForWrap nx ny d =
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

    let rec march x y n d = 
        if n = 0 then x, y
        else
            let nx, ny = 
                match d with
                | 0 -> x + 1, y
                | 1 -> x, y + 1
                | 2 -> x - 1, y
                | _ -> x, y - 1
            let nx, ny = checkForWrap nx ny d
            if map[ny][nx] = '#' then x, y
            else march nx ny (n - 1) d

    let rec crawler x y d =
        function
        | [] -> ((y + 1) * 1000) + ((x + 1) * 4) + d
        | (Move n)::rem ->
            let x, y = march x y n d
            crawler x y d rem
        | Right::rem ->
            let d = if d = 3 then 0 else d + 1
            crawler x y d rem
        | Left::rem ->
            let d = if d = 0 then 3 else d - 1
            crawler x y d rem
    
    let x = Seq.findIndex ((=) '.') map[0]
    crawler x 0 0 instructions
    

let part2() =
    0