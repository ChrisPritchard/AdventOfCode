let input = Input.value

let splitOn (a: char[]) (s: System.String) = s.Split (a, System.StringSplitOptions.RemoveEmptyEntries)

let races = 
    let lines = splitOn [|'\n'|] input
    let times = splitOn [|' '|] lines[0] |> Array.skip 1 |> Array.map int
    let records = splitOn [|' '|] lines[1] |> Array.skip 1 |> Array.map int
    Array.zip times records

let rec find_wins so_far current_time max_time record = 
    let is_win = (max_time - current_time) * current_time > record
    if is_win then
        find_wins (so_far + 1) (current_time + 1L) max_time record
    else if so_far = 0 then
        find_wins so_far (current_time + 1L) max_time record
    else
        so_far

let total = races |> Array.map (fun (time, record) -> find_wins 0 0L time record |> int64) |> Array.reduce (*)

printfn "Part 1: %d" total

let (longer_time, longerRecord) = 
    let lines = splitOn [|'\n'|] input
    let time = splitOn [|':'|] lines[0] |> fun s -> s[1].Replace(" ", "") |> int64
    let record = splitOn [|':'|] lines[1] |> fun s -> s[1].Replace(" ", "") |> int64
    time, record

let win_chances = find_wins 0 0L longer_time longerRecord

printfn "Part 2: %d" win_chances