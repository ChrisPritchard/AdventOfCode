
open System
open System.IO

let part1 = Seq.sum

let part2 lines = 
    let rec findDup state found =
        function
        | head::remaining ->
                let next = state + head
                if Set.contains next found then next
                else findDup next (Set.add next found) remaining
        | [] -> findDup state found lines
     
    findDup 0 (Set.empty.Add(0)) lines

[<EntryPoint>]
let main _ =
    let lines = 
        File.ReadAllLines "input.txt" 
        |> Array.map (Int32.Parse)
        |> Array.toList
    printfn "total lines in input: %i" <| Seq.length lines

    printfn "%i" <| part1 lines
    printfn "%i" <| part2 lines
    0