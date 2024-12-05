let input = System.IO.File.ReadAllLines "input.txt"

let rules =
    input
    |> Array.filter (fun line -> line.Contains "|")
    |> Array.map (fun line -> line.Split("|") |> Array.map int |> (fun parts -> parts[0], parts[1]))
    |> Array.groupBy fst
    |> Array.map (fun (key, vals) -> key, Array.map snd vals)

let lines =
    input
    |> Array.filter (fun line -> line.Contains ",")
    |> Array.map (fun line ->
        line.Split(",")
        |> Array.map int
        |> Array.indexed
        |> Array.map (fun (i, v) -> v, i)
        |> Map.ofArray)

let check key vals line =
    if Map.containsKey key line then
        vals
        |> Array.forall (fun value -> not (Map.containsKey value line) || line[value] > line[key])
    else
        true

let valid, invalid =
    lines
    |> Array.partition (fun line -> rules |> Array.forall (fun (key, vals) -> check key vals line))

let mid_value line =
    let middle_index = Map.count line / 2
    Map.findKey (fun _ i -> i = middle_index) line

let sum =
    valid
    |> Array.filter (fun line -> rules |> Array.forall (fun (key, vals) -> check key vals line))
    |> Array.sumBy mid_value

printfn "Part 1: %A" sum

let rec correct line =
    let invalid_rule =
        rules |> Array.tryFind (fun (key, vals) -> not (check key vals line))

    match invalid_rule with
    | None -> mid_value line
    | Some(key, vals) ->
        let invalid_place = line[key]

        let target =
            vals |> Array.find (fun v -> Map.containsKey v line && line[v] < invalid_place)

        let swap_key = Map.add key line[target] line
        let swap_target = Map.add target invalid_place swap_key
        correct swap_target

let new_sum = invalid |> Array.sumBy correct

printfn "Part 2: %A" new_sum
