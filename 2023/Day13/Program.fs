let input = Input.value

let grids = 
    input.Split "\n\n" |> Array.map (fun grid -> 
        let lines = grid.Split [|'\n'|]
        let down = lines |> Array.map (fun s -> System.Convert.ToInt32(s.Replace('.', '0').Replace('#', '1'), 2))
        let across = [|0..lines[0].Length - 1|] |> Array.map (fun i -> 
            let s = String.init lines.Length (fun j -> if lines[j][i] = '.' then "0" else "1")
            System.Convert.ToInt32(s, 2))
        down, across)

let mirror (grid: int[]): int option =
    None