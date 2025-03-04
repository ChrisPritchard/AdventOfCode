let input = System.IO.File.ReadAllLines "input.txt"

// approach based on optimal paths. since the keypads have limited keys, hard coding the optimal paths is viable
// optimal order is >^v<

let final_keypad =
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

let robot_keypad =
    [ '^', (1, 0); '<', (0, 1); 'v', (1, 1); '>', (2, 1); 'A', (2, 0) ]
    |> Map.ofList

let path_steps (dx, dy) (x, y) keypad =
    

let find_path keypad start_pos to_type =
    to_type
    

let full_path target_sequence =
    find_path final_keypad (2, 3) target_sequence
    |> find_path robot_keypad (2, 0)
    |> find_path robot_keypad (2, 0)

let sub_result (target_sequence: string) =
    let index_num = System.Int32.Parse target_sequence[0 .. target_sequence.Length - 2]
    let full_path = full_path target_sequence
    printfn "%s: %d * %d = %d, %s" target_sequence index_num full_path.Length (index_num * full_path.Length) full_path
    index_num * full_path.Length

printfn "Part 1: %d" (Array.sumBy sub_result input)
