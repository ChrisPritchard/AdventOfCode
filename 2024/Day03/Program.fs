open System.Text.RegularExpressions

let input = System.IO.File.ReadAllLines "input.txt"

let all_input = System.String.Concat(input)

let regex = Regex("mul\(\d*,\d*\)")

let muls =
    regex.Matches(all_input)
    |> Seq.cast<Match>
    |> Seq.map (fun m ->
        let res =
            m.Value.Split("mul(,)".ToCharArray(), System.StringSplitOptions.RemoveEmptyEntries)
            |> Array.map int
            |> Array.reduce (*)

        m.Index, res)
    |> Seq.toList

let sum = muls |> Seq.sumBy snd

printfn "Part 1: %d" sum

let new_regex = Regex("don't\(\)|do\(\)")

let enablers =
    new_regex.Matches(all_input)
    |> Seq.cast<Match>
    |> Seq.map (fun m -> m.Index, m.Value = "do()")
    |> Seq.toArray

let new_sum =
    muls
    |> Seq.filter (fun (i, mul) ->
        let next_enabler =
            Array.tryFindIndex (fun (j, _) -> i < j) enablers
            |> Option.defaultValue enablers.Length

        next_enabler = 0 || snd enablers[next_enabler - 1])
    |> Seq.sumBy snd

printfn "Part 2: %d" new_sum
