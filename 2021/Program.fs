open Common


[<EntryPoint>]
let main argv =
    
    printfn "ADVENT OF CODE 2021"
    printfn "==================="
    printfn ""
    
    time (fun () -> 
        timeForDay 1 1 Day01.part1
        timeForDay 1 2 Day01.part2

        0
    ) |> fun (_, el) -> printfn "total time elapsed: %d ms" el

    0