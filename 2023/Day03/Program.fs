let input = Input.value

let lines = input.Split [|'\n'|] |> Array.map (fun s -> s.ToCharArray())

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
    test_and_reset ()

printfn "Part 1: %d" sum