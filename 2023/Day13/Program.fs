let input = Input.value

let horizontal_lines (grid: string[]) = 
    grid |> Array.map (fun s -> System.Convert.ToInt32(s.Replace('.', '0').Replace('#', '1'), 2))

let vertical_lines (grid: string[]) = 
    [|0..grid[0].Length - 1|] |> Array.map (fun i -> 
            let s = String.init grid.Length (fun j -> if grid[j][i] = '.' then "0" else "1")
            System.Convert.ToInt32(s, 2))

let grids = 
    input.Split "\n\n" |> Array.map (fun grid -> 
        let lines = grid.Split [|'\n'|]
        vertical_lines lines, horizontal_lines lines)

let mirror (grid: int[]): int option =
    [0..grid.Length - 2] |> Seq.tryFind (fun i -> 
        let list_back = [i..(-1)..0]
        let list_ford = [i+1..grid.Length - 1]
        Seq.zip list_back list_ford |> Seq.forall (fun (i, j) -> grid[i] = grid[j]))
    |> Option.map (fun n -> n + 1)

let part1 = grids |> Array.sumBy (fun (vertical_lines, horizontal_lines) -> 
    let horizontal_score = mirror horizontal_lines |> Option.map (fun n -> n * 100) |> Option.defaultValue 0
    let vertical_score = mirror vertical_lines |> Option.defaultValue 0
    horizontal_score + vertical_score)

printfn "Part 1: %d" part1

let mirror_with_exclude exclude (grid: int[]): int option =
    [0..grid.Length - 2] |> Seq.filter (fun i -> i + 1 <> exclude) |> Seq.tryFind (fun i -> 
        let list_back = [i..(-1)..0]
        let list_ford = [i+1..grid.Length - 1]
        Seq.zip list_back list_ford |> Seq.forall (fun (i, j) -> grid[i] = grid[j]))
    |> Option.map (fun i -> i + 1)

let find_smudge (grid: string[]) = 
    let orig_vert = vertical_lines grid |> mirror |> Option.defaultValue -1
    let orig_hori = horizontal_lines grid |> mirror |> Option.defaultValue -1

    let grid = grid |> Array.map (fun s -> s.ToCharArray())
    let mutable i, j = 0, 0
    let mutable found = None

    while i < grid.Length && found = None do
        j <- 0
        while j < grid[i].Length && found = None do
            grid[i][j] <- if grid[i][j] = '#' then '.' else '#'
            let for_mapping = grid |> Array.map (fun a -> System.String a)
            let horizontal_lines = horizontal_lines for_mapping
            match mirror_with_exclude orig_hori horizontal_lines with 
            | Some n ->
                found <- Some (n * 100)
            | _ -> 
                let vertical_lines = vertical_lines for_mapping
                match mirror_with_exclude orig_vert vertical_lines with 
                | Some n ->
                    found <- Some n
                | _ -> grid[i][j] <- if grid[i][j] = '#' then '.' else '#'
            j <- j + 1
        i <- i + 1
    found.Value

let raw_grids = input.Split "\n\n" |> Array.map (fun s -> s.Split [|'\n'|])

let part2 = raw_grids |> Array.sumBy find_smudge

printfn "Part 2: %d" part2