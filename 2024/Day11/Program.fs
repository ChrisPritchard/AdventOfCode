let input = System.IO.File.ReadAllLines "input.txt"

let mutable stones = input[0].Split(" ") |> Array.map int64

let split n =
    let num = n.ToString()
    let left = num[0 .. num.Length / 2 - 1].TrimStart([| '0' |])
    let right = num[num.Length / 2 ..].TrimStart([| '0' |])

    [| if left = "" then 0L else System.Int64.Parse left
       if right = "" then 0L else System.Int64.Parse right |]

let memo = System.Collections.Generic.Dictionary<int64, int64[]>()

let blink num =
    if memo.ContainsKey num then
        memo[num]
    else
        let result =
            match num with
            | 0L -> [| 1L |]
            | n when (int (System.Math.Floor(System.Math.Log10(float n))) + 1) % 2 = 0 -> split n
            | n -> [| n * 2024L |]

        memo.Add(num, result)
        result

for _ = 1 to 25 do
    stones <- Array.collect blink stones

printfn "Part 1: %d" stones.Length

let counts = input[0].Split(" ") |> Array.map int64
let mutable dict = System.Collections.Generic.Dictionary<int64, int64>()

for n in counts do
    dict[n] <- if dict.ContainsKey n then dict[n] + 1L else 1L

for _ in 1..75 do
    let new_dict = System.Collections.Generic.Dictionary<int64, int64>()

    for k in dict.Keys do
        for n in blink k do
            new_dict[n] <-
                if new_dict.ContainsKey n then
                    new_dict[n] + dict[k]
                else
                    dict[k]

    dict <- new_dict

let new_sum = dict.Values |> Seq.sum
printfn "Part 2: %d" new_sum
