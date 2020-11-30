// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp

open Common


[<EntryPoint>]
let main argv =
    
    printfn "ADVENT OF CODE 2020"
    printfn "==================="
    printfn ""
    
    time 1 1 Day01.part1
    time 1 2 Day01.part2

    0 // return an integer exit code