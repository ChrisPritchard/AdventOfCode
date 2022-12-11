open Common

[<EntryPoint>]
let main _ =
    
    printfn ""
    printfn "ADVENT OF CODE 2022"
    printfn "==================="
    printfn ""
    
    let mutable total = 0.0
    
    total <- totalTimeForDay 1 Day01.part1And2
    
    total <- total + timeForDay 2 1 Day02.part1
    total <- total + timeForDay 2 2 Day02.part2
    
    total <- total + timeForDay 3 1 Day03.part1
    total <- total + timeForDay 3 2 Day03.part2
    
    total <- total + timeForDay 4 1 Day04.part1
    total <- total + timeForDay 4 2 Day04.part2
    
    total <- total + timeForDay 5 1 Day05.part1
    total <- total + timeForDay 5 2 Day05.part2

    total <- totalTimeForDay 6 Day06.part1And2

    total <- totalTimeForDay 7 Day07.part1And2

    total <- total + timeForDay 8 1 Day08.part1
    total <- total + timeForDay 8 2 Day08.part2

    total <- total + timeForDay 9 1 Day09.part1
    total <- total + timeForDay 9 2 Day09.part2

    total <- total + timeForDay 10 1 Day10.part1
    total <- total + timeForDay 10 2 Day10.part2

    total <- total + timeForDay 11 1 Day11.part1
    total <- total + timeForDay 11 2 Day11.part2

    printfn ""
    printfn "total time elapsed: %f ms" total
    printfn ""

    0