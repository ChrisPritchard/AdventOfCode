let input = System.IO.File.ReadAllLines "input.txt"

// approach based on optimal paths. since the keypads have limited keys, hard coding the optimal paths is viable
// optimal order is >^v<

let final_steps a b =
    let keypad =
        [ '7', (0, 0)
          '8', (1, 0)
          '9', (2, 0)
          '4', (0, 1)
          '5', (1, 1)
          '6', (2, 1)
          '1', (0, 2)
          '2', (1, 2)
          '3', (2, 2)
          '0', (1, 3)
          'A', (2, 3) ]
        |> Map.ofList

    let x1, y1 = keypad[a]
    let x2, y2 = keypad[b]

    seq {
        if y1 = 3 && x2 = 0 || x1 = 0 && y2 = 3 then
            if y2 < y1 then
                yield! System.String('^', y1 - y2).ToCharArray()

            if x2 > x1 then
                yield! System.String('>', x2 - x1).ToCharArray()

            if y2 > y1 then
                yield! System.String('v', y2 - y1).ToCharArray()

            if x2 < x1 then
                yield! System.String('<', x1 - x2).ToCharArray()
        else
            if x2 < x1 then
                yield! System.String('<', x1 - x2).ToCharArray()

            if y2 > y1 then
                yield! System.String('v', y2 - y1).ToCharArray()

            if y2 < y1 then
                yield! System.String('^', y1 - y2).ToCharArray()

            if x2 > x1 then
                yield! System.String('>', x2 - x1).ToCharArray()
    }
    |> fun c -> new System.String(Array.ofSeq c) + "A"

let mid_steps a b =
    match a, b with
    | '<', '>' -> ">>"
    | '<', 'A' -> ">>^"
    | '<', '^' -> ">^"
    | '<', 'v' -> ">"
    | '>', '<' -> "<<"
    | '>', 'A' -> "^"
    | '>', 'v' -> "<"
    | '>', '^' -> "<^"
    | '^', 'v' -> "v"
    | '^', 'A' -> ">"
    | '^', '>' -> "v>"
    | '^', '<' -> "v<"
    | 'v', '^' -> "^"
    | 'v', 'A' -> "^>"
    | 'v', '<' -> "<"
    | 'v', '>' -> ">"
    | 'A', '<' -> "v<<"
    | 'A', '>' -> "v"
    | 'A', '^' -> "<"
    | 'A', 'v' -> "<v"
    | a, b when a = b -> ""
    | _ -> failwithf "unknown input: %c %c" a b
    |> fun s -> s + "A"

let find_path keypad to_type =
    let rec path_steps acc a rem =
        match rem with
        | "" -> acc
        | s ->
            let new_acc = acc + keypad a s[0]
            path_steps new_acc s[0] s[1..]

    path_steps "" 'A' to_type

let full_path robot_count target_sequence =
    let final_keypad = find_path final_steps target_sequence

    let rec robot_keypads n acc =
        if n = 0 then
            acc
        else
            printfn "robot n = %d" n
            robot_keypads (n - 1) (find_path mid_steps acc)

    robot_keypads robot_count final_keypad

let sub_result robot_count (target_sequence: string) =
    let index_num = System.Int32.Parse target_sequence[0 .. target_sequence.Length - 2]
    let full_path = full_path robot_count target_sequence
    // printfn "%s: %d * %d = %d, %s" target_sequence full_path.Length index_num (index_num * full_path.Length) full_path
    index_num * full_path.Length

printfn "Part 1: %d" (Array.sumBy (sub_result 2) input)
printfn "Part 1: %d" (Array.sumBy (sub_result 25) input)
