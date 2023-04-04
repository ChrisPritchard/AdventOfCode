module Day24

open Common

let part1 () =

    let input = readEmbedded "day24"
    let start = ((Seq.findIndex ((=) '.') input[0]), 0)

    let x_out = input[0].Length - 1
    let y_out = input.Length - 1

    let start_storms = 
        input 
        |> Array.indexed 
        |> Seq.collect (fun (y, line) -> 
            line 
            |> Seq.indexed 
            |> Seq.choose (fun (x, c) -> 
                if c = '.' || c = '#' then None 
                else Some (x, y, c))
            )
        |> Set.ofSeq

    let next_storms current = 
        current 
        |> Set.toSeq
        |> Seq.map (fun (x, y, d) ->
            let nx, ny = 
                match d with
                | '^' -> x, y - 1
                | 'v' -> x, y + 1
                | '<' -> x - 1, y
                | '>' | _ -> x + 1, y
            if nx = 0 then x_out - 1, y, d
            else if nx = x_out then 1, y, d
            else if ny = 0 then x, y_out - 1, d
            else if ny = y_out then x, 1, d
            else nx, ny, d)
        |> Set.ofSeq

    let rec climate acc current =
        let new_storms = next_storms current
        if Set.difference new_storms start_storms |> Set.isEmpty then
            List.rev acc |> Array.ofList
        else
            climate (new_storms::acc) new_storms

    let all_storms = climate [start_storms] start_storms

    Array.length all_storms

    // need to conduct some form of bfs, where the current pos has its count included so the current storm state can be detected

let part2 () =
    0
