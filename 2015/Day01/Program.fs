open System.IO

[<EntryPoint>]
let main _ =
    
    let input = File.ReadAllText "input.txt"

    let part1 = input |> Seq.sumBy (function '(' -> 1 | _ -> -1)
    printfn "part 1: %i" part1

    let part2 = 
        input 
        |> Seq.fold (fun (floor, index) c -> 
            if floor < 0 then floor, index
            else
                let next = floor + (match c with '(' -> 1 | _ -> -1)
                next, index + 1) (0, 0)
        |> snd
    printfn "part 2: %i" part2

    0
