open System
open System.IO

[<EntryPoint>]
let main _ =

    let input = File.ReadAllLines "input.txt"
    let packages = input |> Seq.map (Int64.Parse) |> Seq.toList

    let rec combinations maxLength list = 
        match maxLength, list with
        | 0, _ -> [[]]
        | _, [] -> []
        | k, (x::xs) -> 
            List.map ((@) [x]) (combinations (k-1) xs) @ combinations k xs

    let findEntanglement sectionCount =
        let weight = List.sum packages / sectionCount
        [1..List.length packages - 1] 
        |> Seq.map (fun n -> 
            combinations n packages
            |> List.filter (fun set -> List.sum set = weight)
            |> List.sortBy (fun set -> List.length set, set |> List.reduce (*))
            |> List.tryHead)
        |> Seq.pick id
        |> List.reduce (*)

    printfn "part 1: %i" <| findEntanglement 3L
    printfn "part 2: %i" <| findEntanglement 4L

    0
