open Common


[<EntryPoint>]
let main argv =
    
    printfn "ADVENT OF CODE 2021"
    printfn "==================="
    printfn ""
    
    Day01.init()
    timeForDay 1 1 Day01.part1
    timeForDay 1 2 Day01.part2

    printfn ""
    0