let input = System.IO.File.ReadAllLines "input.txt"

let map_break = input |> Array.findIndex (fun line -> line.Trim() = "")

let map = input[0 .. map_break - 1] |> Array.map Seq.toArray

let instructions = input[map_break + 1 ..] |> Array.collect Seq.toArray

let robot_start =
    map
    |> Seq.indexed
    |> Seq.pick (fun (y, line) ->
        line
        |> Seq.indexed
        |> Seq.tryPick (fun (x, c) ->
            if c = '@' then
                map[y][x] <- '.'
                Some(x, y)
            else
                None))

printfn "%A" map
printfn "%A" instructions
printfn "%A" robot_start
