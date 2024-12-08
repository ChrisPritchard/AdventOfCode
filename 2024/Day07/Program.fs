let input = System.IO.File.ReadAllLines "input.txt"

let folder ops_adder acc (line: string) =
    let parts =
        line.Split(": ".ToCharArray(), System.StringSplitOptions.RemoveEmptyEntries)
        |> Array.map int64
        |> List.ofArray

    let rec combo_tester (results: int64 list) numbers =
        match numbers with
        | [] -> results
        | n :: rest ->
            let next_results = results |> List.collect (ops_adder n)
            combo_tester next_results rest

    let all_results = combo_tester [ 1 ] parts[1..]

    if List.contains parts[0] all_results then
        acc + parts[0]
    else
        acc

let sum = Seq.fold (folder (fun n o -> [ o + n; o * n ])) 0L input

printfn "Part 1: %d" sum

let new_sum =
    Seq.fold (folder (fun n o -> [ o + n; o * n; int64 (sprintf "%d%d" o n) ])) 0L input

printfn "Part 2: %d" new_sum
