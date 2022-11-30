open Common


[<EntryPoint>]
let main argv =
    
    printfn ""
    printfn "ADVENT OF CODE 2022"
    printfn "==================="
    printfn ""
    
    let mutable total = 0.0
    
    Day01.init()
    
    total <- total + timeForDay 1 1 Day01.part1
    total <- total + timeForDay 1 2 Day01.part2

    printfn ""
    printfn "total time elapsed: %f ms" total
    printfn ""

    0