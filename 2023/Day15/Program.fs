let input = Input.value

let segments = input.Split [|','|]

let hash (s: string) = 
    let mutable total = 0
    for c in s do
        total <- total + int c
        total <- total * 17
        total <- total % 256
    total

let part1 = segments |> Array.sumBy hash
printfn "Part 1: %d" part1