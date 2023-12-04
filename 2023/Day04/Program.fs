let input = Input.value

let splitOn (s: System.String) (a: char[]) = s.Split (a, System.StringSplitOptions.RemoveEmptyEntries)

let all_cards = 
    input.Split [|'\n'|] 
    |> Array.map (fun line -> 
        let parts = splitOn line [|':';'|'|]
        let winning_numbers = splitOn parts[1] [|' '|] |> Array.map int |> Set.ofArray
        let my_numbers = splitOn parts[2] [|' '|] |> Array.map int |> Set.ofArray
        let shared = Set.intersect winning_numbers my_numbers
        shared.Count)

let sum = all_cards |> Array.sumBy (fun wins -> pown 2 (wins - 1))

printfn "Part 1: %d" sum

let mutable duplicates = Map.empty
let mutable total_cards = 0

for index = 0 to all_cards.Length - 1 do
    let count = if duplicates.ContainsKey index then 1 + duplicates[index] else 1
    if all_cards[index] > 0 then // has a win
        for next = index + 1 to index + all_cards[index] do
            if next < all_cards.Length then
                if duplicates.ContainsKey next then
                    duplicates <- Map.add next (duplicates[next] + count) duplicates
                else
                    duplicates <- Map.add next count duplicates
    total_cards <- total_cards + count

printfn "Part 2: %d" total_cards