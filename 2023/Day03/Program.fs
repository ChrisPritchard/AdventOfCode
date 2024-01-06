let input = System.IO.File.ReadAllLines "input.txt"

let lines = input |> Array.map (fun s -> s.ToCharArray())

let is_number oc = System.Char.IsAsciiDigit oc
let is_symbol oc = oc <> '.' && not (is_number oc)

let has_symbol x y =
    [| -1,-1; 0,-1; 1,-1
       -1,0;        1,0
       -1,1;  0,1;  1,1 |]
    |> Array.map (fun (dx, dy) -> (x + dx, y + dy))
    |> Array.filter (fun (ox, oy) -> ox >= 0 && oy >= 0 && ox < lines[0].Length && oy < lines.Length)
    |> Array.exists (fun (ox, oy) -> is_symbol (lines[oy][ox]))

let mutable sum = 0

for y in 0..lines.Length - 1 do 
    let mutable current_number = ""
    let mutable number_valid = false
    let test_and_reset () =
        if current_number <> "" then
            if number_valid then
                sum <- sum + int current_number
            current_number <- ""
            number_valid <- false
    for x in 0..lines[0].Length - 1 do
        let c = lines[y][x]
        if is_number c then
            current_number <- sprintf "%s%c" current_number c
            if has_symbol x y then
                number_valid <- true
        else test_and_reset ()
    test_and_reset () // end of line numbers

printfn "Part 1: %d" sum

let mutable numbers_with_gears = Set.empty

let get_gear x y =
    [| -1,-1; 0,-1; 1,-1
       -1,0;        1,0
       -1,1;  0,1;  1,1 |]
    |> Array.map (fun (dx, dy) -> (x + dx, y + dy))
    |> Array.filter (fun (ox, oy) -> ox >= 0 && oy >= 0 && ox < lines[0].Length && oy < lines.Length)
    |> Array.tryFind (fun (ox, oy) -> lines[oy][ox] = '*')

for y in 0..lines.Length - 1 do 
    let mutable current_number = ""
    let mutable current_gear = None
    let test_and_reset () =
        if current_number <> "" then
            match current_gear with None -> () | Some (x, y) -> numbers_with_gears <- Set.add ((x, y), int current_number) numbers_with_gears
            current_number <- ""
            current_gear <- None
    for x in 0..lines[0].Length - 1 do
        let c = lines[y][x]
        if is_number c then
            current_number <- sprintf "%s%c" current_number c
            match get_gear x y with None -> () | Some (x, y) -> current_gear <- Some (x, y)
        else test_and_reset ()
    test_and_reset () // end of line numbers

let pairs = 
    numbers_with_gears 
    |> Set.toArray 
    |> Array.groupBy fst // all numbers that share a gear
    |> Array.map snd 
    |> Array.choose (fun numbers -> 
        if numbers.Length <> 2 then None else numbers |> Array.map snd |> Array.reduce (*) |> Some)
    |> Array.map int64
    |> Array.sum

printfn "Part 2: %d" pairs