let input = System.IO.File.ReadAllLines "input.txt"

let mix (secret: uint64) new_number = secret ^^^ new_number
let prune secret = secret % 16777216UL

let next secret =
    secret * 64UL
    |> mix secret
    |> prune
    |> fun s -> s / 32UL |> mix s |> prune
    |> fun s -> s * 2048UL |> mix s |> prune

let rec nth n secret =
    let new_secret = next secret
    if n = 1 then new_secret else nth (n - 1) new_secret

let num_2000 = input |> Array.map System.UInt64.Parse |> Array.sumBy (nth 2000)

printfn "Part 1: %d" num_2000
