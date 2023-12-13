let input = Input.value

let springs = 
    input.Split [|'\n'|] |> Array.map (fun line -> 
        let parts = line.Split [|' '|]

        parts[0], (parts[1].Split [|','|] |> Array.map int))

let make_str (c: char) len = String.init len (fun _ -> c.ToString())

let rec positions total_len integrity =
    if Array.isEmpty integrity then
        if total_len = 0 then [|""|] else [|make_str '.' total_len|]
    else
        let required_space = integrity[1..] |> Array.sumBy (fun n -> n + 1) // e.g. 3,2 would me a min of .###.##, or seven spaces
        let possible_positions = total_len - required_space - (integrity[0] - 1) // e.g. if size is 2, total_len is 6, required is 3, then there should be two possible spaces
        [|0..possible_positions-1|] |> Array.collect (fun pos ->
            let marks = make_str '#' integrity[0];
            let prefix = if pos = 0 then marks else make_str '.' pos + marks
            let full_prefix = if integrity.Length > 1 then prefix + "." else prefix
            positions (total_len - full_prefix.Length) integrity[1..] |> Array.map (fun s -> full_prefix + s))

let part1 = springs |> Array.sumBy (fun (mask, integrity) ->
    let len = mask.Length
    positions len integrity |> Array.filter (fun possible -> Seq.forall2 (fun m p -> m = '?' || m = p) mask possible) |> Array.length)

printfn "Part 1: %d" part1

// let bigger_springs = springs |> Array.map (fun (mask, integrity) -> String.init 4 (fun _ -> mask + "?") + mask, [|0..4|] |> Array.collect (fun _ -> integrity))

// let mask_is c = 

// let rec valid_positions total_len mask integrity =
//     if Array.isEmpty integrity then
//         if total_len = 0 && mask then 1 else if 

// let part2 = bigger_springs |> Array.sumBy (fun (mask, integrity) ->
//     let len = mask.Length
//     positions len integrity |> Array.filter (fun possible -> Seq.forall2 (fun m p -> m = '?' || m = p) mask possible) |> Array.length)

// printfn "Part 2: %d" part2

// could improve this dramatically by not actually creating the strings, and finding someway to filter out by mask as we go
// these are connected: strings only exist so they can be checked by the mask. a mask could be applied to the calculated space, per section