open System

let input = System.IO.File.ReadAllLines "input.txt"

let line_parser (line: string) =
    line.Split([| " " |], StringSplitOptions.RemoveEmptyEntries)
    |> Array.map int
    |> Array.toList

type Last =
    | Lower of int
    | Higher of int
    | Start of int
    | None

let rec line_checker last =
    function
    | n :: next ->
        match last with
        | Lower o when o < n && n - o <= 3 -> line_checker (Lower n) next
        | Higher o when o > n && o - n <= 3 -> line_checker (Higher n) next
        | Start o when o <> n && abs (o - n) <= 3 ->
            if o < n then
                line_checker (Lower n) next
            else
                line_checker (Higher n) next
        | None -> line_checker (Start n) next
        | _ -> false
    | [] -> true

let sum = input |> Array.filter (line_parser >> line_checker None) |> Array.length
printfn "Part 1: %d" sum

let try_remove_a_level line =
    [ 0 .. List.length line ]
    |> List.exists (fun i ->
        let modified_line =
            line |> List.indexed |> List.filter (fun (j, _) -> i <> j) |> List.map snd

        line_checker None modified_line)

let new_sum =
    input
    |> Array.filter (fun raw_line ->
        let line = line_parser raw_line
        line_checker None line || try_remove_a_level line)
    |> Array.length

printfn "Part 2: %d" new_sum
