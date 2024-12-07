let input = System.IO.File.ReadAllLines "input.txt"

let starting_position =
    input
    |> Array.indexed
    |> Array.pick (fun (i, line) ->
        line
        |> Seq.indexed
        |> Seq.tryPick (fun (j, c) -> if c = '^' then Some(j, i) else None))

let blocks =
    input
    |> Array.indexed
    |> Seq.collect (fun (i, line) ->
        line
        |> Seq.indexed
        |> Seq.choose (fun (j, c) -> if c = '#' then Some(j, i) else None))
    |> Set.ofSeq

let delta =
    function
    | 0 -> 0, -1
    | 1 -> 1, 0
    | 2 -> 0, 1
    | _ -> -1, 0

let turn dir = if dir = 3 then 0 else dir + 1

type Pathing =
    | Normal of ((int * int) * int) list
    | Loop

let rec march blockers acc (x, y) dir =

    let (dx, dy) = delta dir
    let nx, ny = x + dx, y + dy

    let add x y dir = ((x, y), dir) :: acc

    if nx < 0 || nx = input[0].Length || ny < 0 || ny = input.Length then
        Normal(List.rev acc)
    else if List.contains ((nx, ny), dir) acc then
        Loop
    else if Set.contains (nx, ny) blockers then
        march blockers acc (x, y) (turn dir)
    else
        march blockers (add nx ny dir) (nx, ny) dir

let path =
    match march blocks [ starting_position, 0 ] starting_position 0 with
    | Normal path -> path
    | _ -> failwith "loop for part 1"

let sum = List.map fst path |> Set.ofList |> Set.count
printfn "Part 1: %d" sum

let path_array = Array.ofList path

let folder (loops, acc, blockers_tested) index =
    let new_acc = path_array[index] :: acc

    if snd path[index] <> snd path_array[index + 1] then
        loops, new_acc, blockers_tested // skip existing turns
    else
        // printfn "testing %A" path_array[index]

        let (x, y), dir = path_array[index]
        let ahead = let dx, dy = delta dir in x + dx, y + dy

        if Set.contains ahead blockers_tested then
            loops, new_acc, blockers_tested
        else
            let new_blockers_tested = Set.add ahead blockers_tested
            let custom_blocks = Set.add ahead blocks
            let new_dir = turn dir

            match march custom_blocks new_acc (x, y) new_dir with
            | Loop ->
                // printfn "loop found with blocker at %A" ahead
                loops + 1, new_acc, new_blockers_tested
            | _ -> loops, new_acc, new_blockers_tested

let loops, _, _ =
    [ 0 .. path_array.Length - 2 ] |> List.fold folder (0, [], Set.empty)

printfn "Part 2: %d" loops
