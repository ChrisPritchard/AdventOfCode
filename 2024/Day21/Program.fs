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

    let rep symbol count =
        System.String(symbol, count).ToCharArray()

    seq {
        if y1 = 3 && x2 = 0 || x1 = 0 && y2 = 3 then // different order if there is a chance of passing through the dead tile in the bottom left
            if y2 < y1 then
                yield! rep '^' (y1 - y2)

            if x2 > x1 then
                yield! rep '>' (x2 - x1)

            if y2 > y1 then
                yield! rep 'v' (y2 - y1)

            if x2 < x1 then
                yield! rep '<' (x1 - x2)
        else
            if x2 < x1 then
                yield! rep '<' (x1 - x2)

            if y2 > y1 then
                yield! rep 'v' (y2 - y1)

            if y2 < y1 then
                yield! rep '^' (y1 - y2)

            if x2 > x1 then
                yield! rep '>' (x2 - x1)
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

// calculating the full sequence for each mid keypad is not practical once the number of robots is over 10 or so (for part 2, which uses 25)
// instead, we used a depth memo approach: each first step is calculated in turn

// we start with the first character, we find the steps to execute it for a robot, we sub and find the steps for the next robot
// given a sequence to type, and a starting position, we get the path from the mid steps
// we then get the cost by recursively running

let full_path robot_count target_sequence = ""

let sub_result robot_count (target_sequence: string) =
    let index_num = System.Int32.Parse target_sequence[0 .. target_sequence.Length - 2]
    let full_path = full_path robot_count target_sequence
    // printfn "%s: %d * %d = %d, %s" target_sequence full_path.Length index_num (index_num * full_path.Length) full_path
    index_num * full_path.Length

printfn "Part 1: %d" (Array.sumBy (sub_result 2) input)
printfn "Part 1: %d" (Array.sumBy (sub_result 25) input)
