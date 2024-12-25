let input = System.IO.File.ReadAllLines "input.txt"

let patterns =
    input[0].Split(" ,".ToCharArray(), System.StringSplitOptions.RemoveEmptyEntries)

let towels = input[2..]

let rec matcher progress =
    let next =
        progress
        |> Array.collect (fun (rem: string) ->
            patterns
            |> Array.choose (fun p -> if rem.StartsWith p then Some(rem[p.Length ..]) else None))
        |> Array.distinct

    if Array.isEmpty next then
        false
    else if Array.exists (fun (rem: string) -> rem = "") next then
        true
    else
        matcher next

let count = towels |> Array.filter (fun t -> matcher [| t |]) |> Array.length

printfn "Part 1: %d" count
