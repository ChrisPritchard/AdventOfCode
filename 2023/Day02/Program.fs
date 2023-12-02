let input = Input.value

let games = 
    input.Split [|'\n'|] |> Array.map (fun line -> 
        line.Split [|':';';'|] |> fun segments -> 
            let num = segments[0].Substring(5) |> int32
            let draws = segments[1..] |> Array.map (fun draw -> 
                draw.Split [|' ';','|] |> Array.filter ((<>) "") |> Array.chunkBySize 2 |> Array.map (fun pair -> pair[1], int pair[0]) |> dict)
            num, draws)

let mutable sum = 0

for (game, draws) in games do
    if draws |> Array.forall (fun draw -> 
        let key s = if draw.ContainsKey s then draw[s] else 0
        key "red" <= 12 && key "green" <= 13 && key "blue" <= 14) then
        sum <- sum + game

printfn "Part 1: %d" sum

sum <- 0

for (_, draws) in games do
    let mutable minRed = 0;
    let mutable minGreen = 0;
    let mutable minBlue = 0;
    for draw in draws do
        if draw.ContainsKey "red" && draw["red"] > minRed then minRed <- draw["red"]
        if draw.ContainsKey "green" && draw["green"] > minGreen then minGreen <- draw["green"]
        if draw.ContainsKey "blue" && draw["blue"] > minBlue then minBlue <- draw["blue"]
    sum <- sum + (minRed * minGreen * minBlue)

printfn "Part 2: %d" sum