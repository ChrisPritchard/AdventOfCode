let input = System.IO.File.ReadAllLines "input.txt"

let mutable stones = input[0].Split(" ")

let split (num: string) =
    let left = num[0 .. num.Length / 2 - 1].TrimStart([| '0' |])
    let right = num[num.Length / 2 ..].TrimStart([| '0' |])

    [| if left = "" then "0" else left
       if right = "" then "0" else right |]

let memo = System.Collections.Generic.Dictionary<string, string[]>()

let blink num =
    if memo.ContainsKey num then
        memo[num]
    else
        let result =
            match num with
            | "0" -> [| "1" |]
            | n when n.Length % 2 = 0 -> split n
            | n -> [| (System.Int64.Parse(n) * 2024L).ToString() |]

        memo.Add(num, result)
        result

for _ = 1 to 25 do
    let prelen = stones.Length
    stones <- Array.collect blink stones
    printfn "  %d" (stones.Length - prelen)
    printfn "%d" stones.Length

printfn "Part 1: %d" stones.Length

let mutable initial_stones = input[0].Split(" ")[1..1]

let mutable sum = 0L

for stone in initial_stones do
    let mutable stone_set = [| stone |]
    let mutable last_size = 0

    while stone_set.Length - last_size > 0 || stone_set.Length < 1000 do
        last_size <- stone_set.Length
        stone_set <- Array.collect blink stone_set
        printfn "%d" stone_set.Length

    sum <- sum + int64 stone_set.Length

printfn "Part 2: %d" sum
