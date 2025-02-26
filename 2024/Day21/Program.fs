let input = System.IO.File.ReadAllLines "input.txt"

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

let rec path_step_options (dx, dy) (x, y) keypad acc =
    let valid p = Map.exists (fun _ v -> v = p) keypad

    seq {
        if dx < x && valid (dx + 1, dy) then
            yield! path_step_options (dx + 1, dy) (x, y) keypad (acc + ">")
        else if dx > x && valid (dx - 1, dy) then
            yield! path_step_options (dx - 1, dy) (x, y) keypad (acc + "<")

        if dy < y && valid (dx, dy + 1) then
            yield! path_step_options (dx, dy + 1) (x, y) keypad (acc + "v")
        else if dy > y && valid (dx, dy - 1) then
            yield! path_step_options (dx, dy - 1) (x, y) keypad (acc + "^")

        if dx = x && dy = y then
            yield acc + "A"
    }
    |> Seq.toArray


let find_path keypad start_pos (to_type_options: string[]) =
    to_type_options
    |> Array.collect (fun to_type ->
        let mutable acc = [| "" |]

        let mutable pos = start_pos

        for c in to_type do
            let target = Map.find c keypad

            acc <-
                acc
                |> Array.collect (fun so_far ->
                    path_step_options pos target keypad "" |> Array.map (fun s -> so_far + s))

            pos <- target

        acc)

let full_path target_sequence =
    find_path final_keypad (2, 3) [| target_sequence |]
    |> find_path robot_keypad (2, 0)
    |> find_path robot_keypad (2, 0)
    |> Array.minBy (fun s -> s.Length)

let sub_result (target_sequence: string) =
    let index_num = System.Int32.Parse target_sequence[0 .. target_sequence.Length - 2]
    let full_path = full_path target_sequence
    printfn "%s: %d * %d = %d, %s" target_sequence index_num full_path.Length (index_num * full_path.Length) full_path
    index_num * full_path.Length

printfn "Part 1: %d" (Array.sumBy sub_result input)
