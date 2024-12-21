let input = System.IO.File.ReadAllLines "input.txt"

let graph =
    input
    |> Seq.indexed
    |> Seq.collect (fun (y, line) ->
        line
        |> Seq.indexed
        |> Seq.filter (fun (_, c) -> c <> '#')
        |> Seq.map (fun (x, _) ->
            let point = (x, y)

            let neighbours =
                seq {
                    if input[y][x - 1] <> '#' then
                        yield '<', (x - 1, y)

                    if input[y][x + 1] <> '#' then
                        yield '>', (x + 1, y)

                    if input[y - 1][x] <> '#' then
                        yield '^', (x, y - 1)

                    if input[y + 1][x] <> '#' then
                        yield 'v', (x, y + 1)
                }

            point, Array.ofSeq neighbours))
    |> Map.ofSeq

let neighbours (x, y) dir =
    let neighbours = graph[x, y]

    let invalid =
        match dir with
        | '>' -> '<'
        | '<' -> '>'
        | '^' -> 'v'
        | _ -> '^'

    neighbours
    |> Array.filter (fun (d, _) -> d <> invalid)
    |> Array.map (fun (d, p) -> if d = dir then 1, p else 1000, p)

let start = (1, input.Length - 2)
let target = (input[0].Length - 2, 1)
