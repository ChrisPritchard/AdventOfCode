module Day08

open Common
open System

let part1 () =
    let grid = readEmbedded "day08"
    
    let outside = grid.Length * 2 + (grid[0].Length - 2) * 2 

    let rec sightTester visible lastHighest (x, y) (dx, dy) =
        let visible, heighest =
            let height = grid[y][x]
            if height > lastHighest then
                Set.add (x, y) visible, height
            else
                visible, lastHighest
        if heighest = '9' then visible
        else
            let (nx, ny) = (x + dx, y + dy)
            if nx = 0 || ny = 0 || nx = grid[0].Length - 1 || ny = grid.Length - 1 then visible
            else sightTester visible heighest (nx, ny) (dx, dy)

    let horizontal = 
        (Set.empty, [1..grid.Length - 2])
        ||> List.fold (fun visible row ->
            let half1 = sightTester visible (grid[row][0]) (1, row) (1, 0)
            let half2 = sightTester half1 (grid[row][grid[0].Length - 1]) (grid[0].Length - 2, row) (-1, 0)
            half2)
    let withVertical = 
        (horizontal, [1..grid[0].Length - 2])
        ||> List.fold (fun visible col ->
            let half1 = sightTester visible (grid[0][col]) (col, 1) (0, 1)
            let half2 = sightTester half1 (grid[grid.Length - 1][col]) (col, grid.Length - 2) (0, -1)
            half2)

    Set.count withVertical + outside

let part2() =
    0