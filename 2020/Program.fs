open Common


[<EntryPoint>]
let main argv =
    
    printfn "ADVENT OF CODE 2020"
    printfn "==================="
    printfn ""
    
    time (fun () -> 
        timeForDay 1 1 Day01.part1
        timeForDay 1 2 Day01.part2

        timeForDay 2 1 Day02.part1
        timeForDay 2 2 Day02.part2

        timeForDay 3 1 Day03.part1
        timeForDay 3 2 Day03.part2

        timeForDay 4 1 Day04.part1
        timeForDay 4 2 Day04.part2

        timeForDay 5 1 Day05.part1
        timeForDay 5 2 Day05.part2

        0
    ) |> fun (_, el) -> printfn "total time elapsed: %d ms" el

    0