let input = System.IO.File.ReadAllLines "input.txt"

let pat_width = 5
let pat_height = 7

let keys, locks =
    input
    |> Array.chunkBySize (pat_height + 1)
    |> Array.map (fun lines ->
        let is_key = lines[0][0] = '#'

        let heights =
            [| 0 .. pat_width - 1 |]
            |> Array.map (fun x ->
                if is_key then
                    [| 0 .. pat_height - 2 |]
                else
                    [| pat_height - 1 .. -1 .. 0 |]
                |> Array.filter (fun y -> lines[y][x] = '#')
                |> Array.length)

        is_key, heights)
    |> Array.partition fst

let matches =
    keys
    |> Array.sumBy (fun (_, key) ->
        locks
        |> Array.filter (fun (_, lock) -> Array.zip key lock |> Array.forall (fun (a, b) -> a + b <= pat_height))
        |> Array.length)

printfn "Final day: %d" matches
