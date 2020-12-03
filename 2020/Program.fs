// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp

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

        0
    ) |> fun (_, el) -> printfn "total time elapsed: %d ms" el

    0 // return an integer exit code