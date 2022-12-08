module Day08

open Common

let part1 () =
    let grid = readEmbedded "day08"
    
    let outside = grid.Length * 2 + (grid[0].Length - 2) * 2 

    let rec sightTester lastHighest (x, y) (dx, dy) visible =
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
            else sightTester heighest (nx, ny) (dx, dy) visible

    let horizontal = 
        (Set.empty, [1..grid.Length - 2])
        ||> List.fold (fun visible row ->
            visible 
            |> sightTester (grid[row][0]) (1, row) (1, 0) 
            |> sightTester (grid[row][grid[0].Length - 1]) (grid[0].Length - 2, row) (-1, 0))
    let withVertical = 
        (horizontal, [1..grid[0].Length - 2])
        ||> List.fold (fun visible col ->
            visible 
            |> sightTester (grid[0][col]) (col, 1) (0, 1) 
            |> sightTester (grid[grid.Length - 1][col]) (col, grid.Length - 2) (0, -1))

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
            crawl height 0 (x, y) (-1, 0) * 
            crawl height 0 (x, y) (1, 0) * 
            crawl height 0 (x, y) (0, -1) * 
            crawl height 0 (x, y) (0, 1)))
            |> Seq.max