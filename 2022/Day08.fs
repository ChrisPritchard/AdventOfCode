module Day08

open Common
open System

let part1And2 () =
    let grid = readEmbedded "day08"
    
    let outside = grid.Length * 2 + (grid[0].Length - 2) * 2 

    let midAcross = grid[0].Length / 2
    let midDown = grid.Length / 2

    let rec sightTester visible lastHighest (x, y) (lastx, lasty) (dx, dy) =
        if (x, y) = (lastx, lasty) then visible
        else
            let height = grid[y][x]
            if height <= lastHighest || Set.contains (x, y) visible then
                sightTester visible lastHighest (x + dx, y + dy) (lastx, lasty) (dx, dy)
            else 
                sightTester (Set.add (x, y) visible) height (x + dx, y + dy) (lastx, lasty) (dx, dy)

    let horizontal = 
        (Set.empty, [1..grid.Length - 2])
        ||> List.fold (fun visible row ->
            let half1 = sightTester visible (grid[0][row]) (1, row) (midAcross, row) (1, 0)
            let half2 = sightTester half1 (grid[grid[0].Length - 1][row]) (grid[0].Length - 2, row) (midAcross, row) (-1, 0)
            half2)
    let vertical = 
        (horizontal, [1..grid[0].Length - 2])
        ||> List.fold (fun visible col ->
            let half1 = sightTester visible (grid[col][0]) (col, 1) (col, midDown) (0, 1)
            let half2 = sightTester half1 (grid[col][grid.Length - 1]) (col, grid.Length - 2) (col, midDown) (0, -1)
            half2)

    let visible = Set.count vertical + outside


    // for each row and column...
    // proceed to halfway...
    // start with edge - read in and add whenever a tree is higher than last highest
    // if hitting 9 then stop as last visible

    visible, outside
