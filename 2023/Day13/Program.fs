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

let part1 = grids |> Array.sumBy (fun (vertical_line, horizontal_line) -> 
    match mirror horizontal_line with
    | Some n -> 100 * n
    | None -> match mirror vertical_line with | Some n -> n | _ -> 0)

printfn "Part 1: %d" part1