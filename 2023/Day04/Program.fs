let input = System.IO.File.ReadAllLines "input.txt"

let splitOn (s: System.String) (a: char[]) = s.Split (a, System.StringSplitOptions.RemoveEmptyEntries)

let all_cards = 
    input 
    |> Array.map (fun line -> 
        let parts = splitOn line [|':';'|'|]
        let winning_numbers = splitOn parts[1] [|' '|] |> Array.map int |> Set.ofArray
        let my_numbers = splitOn parts[2] [|' '|] |> Array.map int |> Set.ofArray
        let shared = Set.intersect winning_numbers my_numbers
        shared.Count)

let sum = all_cards |> Array.sumBy (fun wins -> pown 2 (wins - 1))

printfn "Part 1: %d" sum

let mutable times_to_run = [0..all_cards.Length - 1] |> Seq.map (fun i -> i, 1) |> Map.ofSeq
let mutable total_cards = 0

for index = 0 to all_cards.Length - 1 do
    let count = times_to_run[index]
    if all_cards[index] > 0 then // has a win
        for next = index + 1 to index + all_cards[index] do
            if next < all_cards.Length then
                times_to_run <- Map.add next (times_to_run[next] + count) times_to_run
    total_cards <- total_cards + count

printfn "Part 2: %d" total_cards