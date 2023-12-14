let input = Input.value

let grid = input.Split [|'\n'|] |> Array.map (fun s -> s.ToCharArray())

for y in 0..grid.Length - 1 do
    for x in 0..grid[y].Length - 1 do
        if grid[y][x] = 'O' then
            grid[y][x] <- '.'
            let mutable new_pos = y - 1
            while new_pos >= 0 && grid[new_pos][x] = '.' do
                new_pos <- new_pos - 1
            grid[new_pos + 1][x] <- 'O'

let part1 = grid |> Array.rev |> Array.indexed |> Array.sumBy (fun (y, line) -> 
    line |> Array.filter ((=) 'O') |> Array.length |> fun len -> len * (y + 1))

printfn "Part 1: %d" part1
