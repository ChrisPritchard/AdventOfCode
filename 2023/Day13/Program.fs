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
    match mirror horizontal_lines with
    | Some n -> 100 * n
    | None -> match mirror vertical_lines with | Some n -> n | _ -> 0)

printfn "Part 1: %d" part1

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
            let vertical_lines = vertical_lines for_mapping
            match mirror vertical_lines with 
            | Some n when n <> orig_vert ->
                found <- Some n
            | _ -> 
                let horizontal_lines = horizontal_lines for_mapping
                match mirror horizontal_lines with 
                | Some n when n <> orig_hori ->
                    found <- Some (n * 100)
                | _ -> grid[i][j] <- if grid[i][j] = '#' then '.' else '#'
            j <- j + 1
        i <- i + 1
    match found with Some n -> n | _ -> if orig_vert <> -1 then orig_vert else orig_hori

let raw_grids = input.Split "\n\n" |> Array.map (fun s -> s.Split [|'\n'|])

let part2 = raw_grids |> Array.sumBy find_smudge

printfn "Part 2: %d" part2