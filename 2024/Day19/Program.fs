let input = System.IO.File.ReadAllLines "input.txt"

let patterns =
    input[0].Split(" ,".ToCharArray(), System.StringSplitOptions.RemoveEmptyEntries)

let towels = input[2..]

let rec matcher progress =
    let next =
        progress
        |> Array.collect (fun (c, rem: string) ->
            if rem = "" then
                [| c, rem |]
            else
                patterns
                |> Array.choose (fun p -> if rem.StartsWith p then Some(c, rem[p.Length ..]) else None))
        |> Array.groupBy snd
        |> Array.map (fun (remainder, options) -> Array.sumBy fst options, remainder)

    if Array.isEmpty next then
        0UL
    else if Array.length next = 1 && snd next[0] = "" then
        fst next[0]
    else
        matcher next

let valid =
    towels
    |> Array.choose (fun t -> let result = matcher [| 1UL, t |] in if result > 0UL then Some result else None)

let count = valid |> Array.length

printfn "Part 1: %d" count

let total_combos = valid |> Array.sum

printfn "Part 2: %d" total_combos
