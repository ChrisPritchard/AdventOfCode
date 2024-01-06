let input = System.IO.File.ReadAllLines "input.txt"

let lines = input |> Array.map (fun line -> line.Split [|' '|] |> Array.map int64)

let rec find_next (line: int64[]) =
    let diffs = line |> Array.windowed 2 |> Array.map (fun window -> window[1] - window[0])
    if Array.forall ((=) 0L) diffs then line[line.Length - 1]
    else line[line.Length - 1] + find_next diffs

let sum = Array.sumBy find_next lines
printfn "Part 1: %d" sum

let rec find_prev (line: int64[]) =
    let diffs = line |> Array.windowed 2 |> Array.map (fun window -> window[1] - window[0])
    if Array.forall ((=) 0L) diffs then line[0]
    else line[0] - find_prev diffs

let sum2 = Array.sumBy find_prev lines
printfn "Part 2: %d" sum2