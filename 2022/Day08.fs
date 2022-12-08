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
            let fromLeft = sightTester visible (grid[row][0]) (1, row) (1, 0)
            let fromRight = sightTester fromLeft (grid[row][grid[0].Length - 1]) (grid[0].Length - 2, row) (-1, 0)
            fromRight)
    let withVertical = 
        (horizontal, [1..grid[0].Length - 2])
        ||> List.fold (fun visible col ->
            let fromTop = sightTester visible (grid[0][col]) (col, 1) (0, 1)
            let fromBottom = sightTester fromTop (grid[grid.Length - 1][col]) (col, grid.Length - 2) (0, -1)
            fromBottom)

    Set.count withVertical + outside

let part2() =
    let grid = readEmbedded "day08"
    
    let rec crawl maxHeight count (x, y) (dx, dy) =
        let (nx, ny) = (x + dx, y + dy)
        if nx < 0 || ny < 0 || nx = grid[0].Length || ny = grid.Length then count
        else
            let height = grid[ny][nx]
            if height < maxHeight then 
                crawl maxHeight (count + 1) (nx, ny) (dx, dy)
            else 
                count + 1

    [0..grid.Length - 1] |> Seq.collect (fun y ->
        [0..grid[y].Length - 1] |> Seq.map (fun x -> 
            let height = grid[y][x]
            let res = 
                crawl height 0 (x, y) (-1, 0) * 
                crawl height 0 (x, y) (1, 0) * 
                crawl height 0 (x, y) (0, -1) * 
                crawl height 0 (x, y) (0, 1)
            res))
            |> Seq.max