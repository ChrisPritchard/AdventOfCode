let input = System.IO.File.ReadAllLines "input.txt"

let final_keypad =
    function
    | 7 -> 0, 0
    | 8 -> 1, 0
    | 9 -> 2, 0
    | 4 -> 0, 1
    | 5 -> 1, 1
    | 6 -> 2, 1
    | 1 -> 0, 2
    | 2 -> 1, 2
    | 3 -> 2, 2
    | 0 -> 1, 3
    | _ -> 2, 3 // used for 'A' character

let robot_keypad =
    function
    | '^' -> 1, 0
    | '<' -> 0, 1
    | 'v' -> 1, 1
    | '>' -> 2, 1
    | _ -> 2, 0 // used for 'A' charactger

let path_steps (x1, y1) (x2, y2) =
    let mutable result = ""

    let horizontal = x1 - x2

    if horizontal > 0 then
        result <- result + System.String('<', horizontal)
    else
        result <- result + System.String('>', abs horizontal)

    let vertical = y1 - y2

    if vertical > 0 then
        result <- result + System.String('^', vertical)
    else
        result <- result + System.String('v', abs vertical)

    result + "A"

let start_pos = 2, 3

let rec find_path keypad acc pos rem =
    match rem with
    | "" -> acc
    | s ->
        let n = s[0]
        let target = keypad (if n = 'A' then -1 else int n - int '0')
        let new_acc = acc + path_steps pos target
        find_path keypad new_acc target s[1..]

printfn "%A" (find_path final_keypad "" start_pos "029A")
