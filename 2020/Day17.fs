module Day17

open System.IO

let input = File.ReadAllLines "./inputs/day17.txt"

let count = 6
let xlen, ylen = input.Length, input.[0].Length

let nextState c p i adjacent = 
    let active = 
        adjacent p i 
        |> Array.filter (fun s -> s = '#') 
        |> Array.length
    match c with
    | '#' when active <> 2 && active <> 3 -> '.'
    | '.' when active = 3 -> '#'
    | s -> s

let part1 () = 
    let grids = 
        [|0..count|]
        |> Array.map (fun i ->
            Array.init (xlen + count*2) (fun _ ->
                Array.init (ylen + count*2) (fun _ ->
                    Array.init (1 + count*2) (fun _ -> '.'))))
    for x in [0..xlen-1] do
        for y in [0..ylen-1] do
            grids.[0].[x+count].[y+count].[count] <- input.[x].[y]

    let cardinals =
        [|-1..1|] |> Array.collect (fun dx -> 
            [|-1..1|] |> Array.collect (fun dy ->
                [|-1..1|] 
                |> Array.filter (fun dz -> not (dx = 0 && dy = 0 && dz = 0)) 
                |> Array.map (fun dz -> dx, dy, dz)))

    let adjacent (x, y, z) i =
        cardinals 
        |> Array.choose (fun (dx, dy, dz) -> 
            let x, y, z = x + dx, y + dy, z + dz
            if x < 0 || y < 0 || z < 0 then None
            elif x >= grids.[i].Length || y >= grids.[i].[0].Length then None
            elif z >= grids.[i].[0].[0].Length then None
            else Some (grids.[i].[x].[y].[z]))

    let mutable sum = 0
    for i in [0..count-1] do
        for x in [0..grids.[i].Length-1] do
            for y in [0..grids.[i].[0].Length-1] do
                for z in [0..grids.[i].[0].[0].Length-1] do
                    let c = grids.[i].[x].[y].[z]
                    let n = nextState c (x, y, z) i adjacent
                    if i = 5 then
                        if n = '#' then sum <- sum + 1
                    else
                        grids.[i+1].[x].[y].[z] <- n
    sum

let part2 () =
    let grids = 
        [|0..count|]
        |> Array.map (fun i ->
            Array.init (xlen + count*2) (fun _ ->
                Array.init (ylen + count*2) (fun _ ->
                    Array.init (1 + count*2) (fun _ ->
                        Array.init (1 + count*2) (fun _ -> '.')))))
    for x in [0..xlen-1] do
        for y in [0..ylen-1] do
            grids.[0].[x+count].[y+count].[count].[count] <- input.[x].[y]

    let cardinals =
        [|-1..1|] |> Array.collect (fun dx -> 
            [|-1..1|] |> Array.collect (fun dy ->
                [|-1..1|] |> Array.collect (fun dz ->
                    [|-1..1|] 
                    |> Array.filter (fun dw -> not (dx = 0 && dy = 0 && dz = 0 && dw = 0)) 
                    |> Array.map (fun dw -> dx, dy, dz, dw))))

    let adjacent (x, y, z, w) i =
        cardinals 
        |> Array.choose (fun (dx, dy, dz, dw) -> 
            let x, y, z, w = x + dx, y + dy, z + dz, w + dw
            if x < 0 || y < 0 || z < 0 || w < 0 then None
            elif x >= grids.[i].Length || y >= grids.[i].[0].Length then None
            elif z >= grids.[i].[0].[0].Length || w >= grids.[i].[0].[0].[0].Length then None
            else Some (grids.[i].[x].[y].[z].[w]))

    let mutable sum = 0
    for i in [0..count-1] do
        for x in [0..grids.[i].Length-1] do
            for y in [0..grids.[i].[0].Length-1] do
                for z in [0..grids.[i].[0].[0].Length-1] do
                    for w in [0..grids.[i].[0].[0].[0].Length-1] do
                        let c = grids.[i].[x].[y].[z].[w]
                        let n = nextState c (x, y, z, w) i adjacent
                        if i = 5 then
                            if n = '#' then sum <- sum + 1
                        else
                            grids.[i+1].[x].[y].[z].[w] <- n
    sum