let input = Input.value

let springs = 
    input.Split [|'\n'|] |> Array.map (fun line -> 
        let parts = line.Split [|' '|]

        parts[0].ToCharArray (), (parts[1].Split [|','|] |> Array.map int))

let rec positions total_len integrity: int = 
    if Array.isEmpty integrity then 1
    else
        let required_space = integrity |> Array.skip 1 |> Array.sumBy (fun i -> i + 1)
        let possible_positions = total_len - required_space - (integrity[0] - 1)
        [|0..possible_positions-1|] |> Array.sumBy (fun pos -> 
            let new_len = total_len - (pos + integrity[0] + 1)
            positions new_len integrity[1..])

let ptest1 = positions 12 [|3;2;1|]
printfn "%d" ptest1

// ????..???.???#??##?. 1,1,1,1,1,6
let ptest2 = positions 20 [|1;1;1;1;1;6|]
printfn "%d" ptest2

// for any given position, how can we determine if it fits the mask?