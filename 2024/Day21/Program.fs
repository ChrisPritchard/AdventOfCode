let input = System.IO.File.ReadAllLines "input.txt"

let final_keypad =
    function
    | '7' -> 0, 0
    | '8' -> 1, 0
    | '9' -> 2, 0
    | '4' -> 0, 1
    | '5' -> 1, 1
    | '6' -> 2, 1
    | '1' -> 0, 2
    | '2' -> 1, 2
    | '3' -> 2, 2
    | '0' -> 1, 3
    | _ -> 2, 3 // used for 'A' character

let robot_keypad =
    function
    | '^' -> 1, 0
    | '<' -> 0, 1
    | 'v' -> 1, 1
    | '>' -> 2, 1
    | _ -> 2, 0 // used for 'A' charactger

let path_steps (x1, y1) (x2, y2) bad_row =
    let mutable result = ""

    let horizontal = x1 - x2

    let across =
        if horizontal > 0 then
            System.String('<', horizontal)
        else
            System.String('>', abs horizontal)

    let vertical = y1 - y2

    let through =
        if vertical > 0 then
            System.String('^', vertical)
        else
            System.String('v', abs vertical)

    (if y1 = bad_row then through + across else across + through) + "A"

let rec find_path keypad bad_row acc pos rem =
    match rem with
    | "" -> acc
    | s ->
        let n = s[0]
        let target = keypad n
        let new_acc = acc + path_steps pos target bad_row
        find_path keypad bad_row new_acc target s[1..]

let full_path target_sequence =
    find_path final_keypad 3 "" (2, 3) target_sequence
    |> find_path robot_keypad 0 "" (2, 0)
    |> find_path robot_keypad 0 "" (2, 0)

let sub_result (target_sequence: string) =
    let index_num = System.Int32.Parse target_sequence[0 .. target_sequence.Length - 2]
    let full_path = full_path target_sequence
    printfn "%s: %d * %d = %d, %s" target_sequence index_num full_path.Length (index_num * full_path.Length) full_path
    index_num * full_path.Length

printfn "Part 1: %d" (Array.sumBy sub_result input)
