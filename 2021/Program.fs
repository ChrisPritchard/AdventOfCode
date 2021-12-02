open Common


[<EntryPoint>]
let main argv =
    
    printfn "ADVENT OF CODE 2021"
    printfn "==================="
    printfn ""
    
    let mutable total = 0.0
    
    Day01.init()
    
    total <- total + timeForDay 1 1 Day01.part1
    total <- total + timeForDay 1 2 Day01.part2
    
    Day02.init()
        
    total <- total + timeForDay 2 1 Day02.part1
    total <- total + timeForDay 2 2 Day02.part2

    printfn ""
    printfn "total time elapsed: %f ms" total
    printfn ""

    0