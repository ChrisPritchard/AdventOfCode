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

let bigger_springs = springs |> Array.map (fun (mask, integrity) -> String.init 4 (fun _ -> mask + "?") + mask, [|0..4|] |> Array.collect (fun _ -> integrity))

let mask_is c_list (mask: string) = Seq.forall (fun c -> Array.contains c c_list) mask

let mutable memoisation = Map.empty

let rec valid_positions total_len mask integrity =
    if memoisation.ContainsKey (total_len, mask, integrity) then memoisation[(total_len, mask, integrity)]
    else
        if Array.isEmpty integrity then
            if total_len = 0 || mask_is [|'?';'.'|] mask then 1UL 
            else 0UL // mask has some mandatory '#' remaining, so something is in the wrong position
        else
            let required_space = integrity[1..] |> Array.sumBy (fun n -> n + 1)
            let possible_positions = total_len - required_space - (integrity[0] - 1)
            let relevant_mask = mask[0..total_len - required_space]
            let final_val =
                [|0..possible_positions-1|] |> Array.filter (fun pos -> 
                    let start_valid = pos = 0 || mask_is [|'?';'.'|] relevant_mask[0..pos-1]
                    let pos_valid = mask_is [|'?';'#'|] relevant_mask[pos..pos+integrity[0]-1]
                    let next_valid = integrity.Length = 1 || (let c = mask[pos+integrity[0]] in c = '.' || c = '?')
                    start_valid && pos_valid && next_valid
                ) |> Array.sumBy (fun pos ->
                    let size = pos + integrity[0]
                    let full_size = if integrity.Length > 1 then size + 1 else size
                    let rem_mask = mask[full_size..]
                    valid_positions (total_len - full_size) rem_mask integrity[1..])
            memoisation <- Map.add (total_len, mask, integrity) final_val memoisation
            final_val

// let part2 = bigger_springs |> Array.sumBy (fun (mask, integrity) ->
//     let len = mask.Length
//     positions len integrity |> Array.filter (fun possible -> Seq.forall2 (fun m p -> m = '?' || m = p) mask possible) |> Array.length)

// for i in 0..10 do
//     let (mask_orig, integrity_orig) = springs[i]
//     let (mask_big, integrity_big) = bigger_springs[i]
//     printfn "%i:\t%d\t%d" i (valid_positions mask_orig.Length mask_orig integrity_orig) (valid_positions mask_big.Length mask_big integrity_big)

let part2 = bigger_springs |> Array.sumBy (fun (mask, integrity) -> valid_positions mask.Length mask integrity)
printfn "Part 2: %d" part2 // (valid_positions 12 "?###????????" [|3;2;1|])

// could improve this dramatically by not actually creating the strings, and finding someway to filter out by mask as we go
// these are connected: strings only exist so they can be checked by the mask. a mask could be applied to the calculated space, per section