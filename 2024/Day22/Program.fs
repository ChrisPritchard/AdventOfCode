let input = System.IO.File.ReadAllLines "input.txt"

let mix (secret: int64) new_number = secret ^^^ new_number
let prune secret = secret % 16777216L

let next secret =
    secret * 64L
    |> mix secret
    |> prune
    |> fun s -> s / 32L |> mix s |> prune
    |> fun s -> s * 2048L |> mix s |> prune

let rec nth n secret =
    let new_secret = next secret
    if n = 1 then new_secret else nth (n - 1) new_secret

let num_2000 = input |> Array.map System.Int64.Parse |> Array.sumBy (nth 2000)

printfn "Part 1: %d" num_2000

let rec prices n secret =
    seq {
        yield secret % 10L

        if n > 1 then
            yield! prices (n - 1) (next secret)
    }

let patterns n secret =
    let all_prices = prices n secret |> Array.ofSeq

    all_prices
    |> Array.windowed 5
    |> Array.map (fun prices ->
        (prices[1] - prices[0], prices[2] - prices[1], prices[3] - prices[2], prices[4] - prices[3]), prices[4])
    |> Array.distinctBy fst // only the first time each sequence occurs in a price list

let most_bananas n =
    input
    |> Array.map System.Int64.Parse
    |> Array.collect (patterns n)
    |> Array.groupBy fst
    |> Array.map (fun (_, values) -> Array.sumBy snd values)
    |> Array.sortDescending
    |> Array.head

printfn "Part 2: %d" (most_bananas 2000)
