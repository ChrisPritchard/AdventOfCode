let input = Input.value

let grid = input.Split [|'\n'|] |> Array.map (fun s -> s.ToCharArray())

let get_north_load () =
    grid |> Array.rev |> Array.indexed |> Array.sumBy (fun (y, line) -> 
        line |> Array.filter ((=) 'O') |> Array.length |> fun len -> len * (y + 1))

let start_load = get_north_load()

let roll_north () =
    for y in 0..grid.Length - 1 do
        for x in 0..grid[y].Length - 1 do
            if grid[y][x] = 'O' then
                grid[y][x] <- '.'
                let mutable new_pos = y - 1
                while new_pos >= 0 && grid[new_pos][x] = '.' do
                    new_pos <- new_pos - 1
                grid[new_pos + 1][x] <- 'O'

roll_north ()
printfn "Part 1: %d" (get_north_load ())

// for part 2, which needs to have a four way rotation run 1000000000 times, the answer is likely just to find the loop

let roll_south () = 
    for y in grid.Length - 1..(-1)..0 do
        for x in 0..grid[y].Length - 1 do
            if grid[y][x] = 'O' then
                grid[y][x] <- '.'
                let mutable new_pos = y + 1
                while new_pos < grid.Length && grid[new_pos][x] = '.' do
                    new_pos <- new_pos + 1
                grid[new_pos - 1][x] <- 'O'

let roll_west () = 
    for y in 0..grid.Length - 1 do
        for x in 0..grid[y].Length - 1 do
            if grid[y][x] = 'O' then
                grid[y][x] <- '.'
                let mutable new_pos = x - 1
                while new_pos >= 0 && grid[y][new_pos] = '.' do
                    new_pos <- new_pos - 1
                grid[y][new_pos + 1] <- 'O'

let roll_east () = 
    for y in 0..grid.Length - 1 do
        for x in grid[y].Length - 1..(-1)..0 do
            if grid[y][x] = 'O' then
                grid[y][x] <- '.'
                let mutable new_pos = x + 1
                while new_pos < grid[y].Length && grid[y][new_pos] = '.' do
                    new_pos <- new_pos + 1
                grid[y][new_pos - 1] <- 'O'

roll_west()
roll_south()
roll_east()

let mutable loads = Array.zeroCreate 1000
loads[0] <- start_load
loads[1] <- get_north_load()

for i in 2..999 do
    roll_north()
    roll_west()
    roll_south()
    roll_east()
    loads[i] <- get_north_load()

let rec find_loop i =
    let value = loads[i]
    let mutable j = i + 2
    while j < loads.Length && loads[j] <> value do
        j <- j + 1
    if j = loads.Length then
        find_loop (i + 1)
    else
        let mutable k = 1
        while k < j - i && j + k < loads.Length && loads[i+k] = loads[j+k] do
            k <- k + 1
        if k = j - i then
            i, j - i
        else
            find_loop (i + 1)

let (loop_start, loop_length) = find_loop 0
let result_index = (1000000000 - loop_start) % loop_length
let part2 = loads[loop_start + result_index]

printfn "Part 2: %d" part2