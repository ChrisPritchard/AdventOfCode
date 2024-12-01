open System

let input = System.IO.File.ReadAllLines "input.txt" |> List.ofArray

let parser (a, b) (line: String) =
    let parts = line.Split([| " " |], StringSplitOptions.RemoveEmptyEntries)
    (int parts[0]) :: a, (int parts[1]) :: b

let (left, right) =
    List.fold parser ([], []) input |> fun (a, b) -> List.sort a, List.sort b

let sum = List.zip left right |> List.sumBy (fun (a, b) -> abs (a - b))

printfn "Part 1: %d" sum

let occurrances =
    right
    |> List.groupBy id
    |> List.map (fun (a, b) -> a, List.length b)
    |> Map.ofList

let new_sum =
    left
    |> List.sumBy (fun id ->
        match Map.tryFind id occurrances with
        | Some n -> n * id
        | _ -> 0)

printfn "Part 2: %d" new_sum
