let input = Input.value

let splitOn (a: char[]) (s: System.String) = s.Split (a, System.StringSplitOptions.RemoveEmptyEntries)

let hands = 
    input |> splitOn [|'\n'|] |> Array.map (fun line -> let parts = splitOn [|' '|] line in parts[0].ToCharArray(), int parts[1])

let get_groups allow_jokers (a: char[]) = 
    let to_group = 
        if allow_jokers && Array.contains 'J' a then 
            let without = Array.filter ((<>) 'J') a
            if without.Length = 0 then a
            else
                let top = without |> Array.groupBy id |> Array.map (fun (k, g) -> k, Array.length g) |> Array.sortByDescending snd |> Array.head |> fst
                a |> Array.map (fun c -> if c = 'J' then top else c)
        else a
    to_group |> Array.groupBy id |> Array.map (snd >> Array.length) |> Array.sortDescending

let compare_hand allow_jokers (a: char[], _) (b: char[], _) =
    let a_groups = get_groups allow_jokers a
    let b_groups = get_groups allow_jokers b
    if a_groups.Length < b_groups.Length then 1
    else if b_groups.Length < a_groups.Length then -1
    else if a_groups[0] > b_groups[0] then 1
    else if b_groups[0] > a_groups[0] then -1
    else
        let card_order = if allow_jokers then "AKQT98765432J" else "AKQJT98765432"
        let rec by_card i =
            if card_order.IndexOf(a[i]) < card_order.IndexOf(b[i]) then 1
            else if card_order.IndexOf(b[i]) < card_order.IndexOf(a[i]) then -1
            else if i = 4 then 0
            else by_card (i + 1)
        by_card 0

let sorted1 = Array.sortWith (compare_hand false) hands
let score1 = sorted1 |> Array.indexed |> Array.sumBy (fun (i, (_, bet)) -> (i + 1) * bet)

printfn "Part 1: %d" score1

let sorted2 = Array.sortWith (compare_hand true) hands
let score2 = sorted2 |> Array.indexed |> Array.sumBy (fun (i, (_, bet)) -> (i + 1) * bet)

printfn "Part 2: %d" score2
