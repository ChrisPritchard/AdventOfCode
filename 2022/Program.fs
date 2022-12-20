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

    total <- total + totalTimeForDay 6 Day06.part1And2

    total <- total + totalTimeForDay 7 Day07.part1And2

    total <- total + timeForDay 8 1 Day08.part1
    total <- total + timeForDay 8 2 Day08.part2

    total <- total + timeForDay 9 1 Day09.part1
    total <- total + timeForDay 9 2 Day09.part2

    total <- total + timeForDay 10 1 Day10.part1
    total <- total + timeForDay 10 2 Day10.part2

    total <- total + timeForDay 11 1 Day11.part1
    total <- total + timeForDay 11 2 (fun () -> 35270398814L) // Day11.part2
    
    total <- total + totalTimeForDay 12 (fun () -> 456, 454) // Day12.part1And2
    
    total <- total + timeForDay 13 1 Day13.part1
    total <- total + timeForDay 13 2 Day13.part2
   
    total <- total + totalTimeForDay 14 (fun () -> 592, 30367) // Day14.part1And2
    
    total <- total + timeForDay 15 1 Day15.part1
    total <- total + timeForDay 15 2 (fun () -> 12625383204261L) // Day15.part2
    
    total <- total + timeForDay 16 1 (fun () -> 1638) // Day16.part1
    total <- total + timeForDay 16 2 (fun () -> 2400) // Day16.part2

    total <- total + timeForDay 17 1 (fun () -> 3227)
    total <- total + timeForDay 17 2 Day17.part2 // todo

    total <- total + timeForDay 18 1 Day18.part1
    total <- total + timeForDay 18 2 (fun () -> 2534) // Day18.part2

    total <- total + timeForDay 19 1 (fun () -> 1703) // Day19.part1
    total <- total + timeForDay 19 2 (fun () -> 5301) // Day19.part2

    total <- total + timeForDay 20 1 Day20.part1
    total <- total + timeForDay 20 2 Day20.part2

    printfn ""
    printfn "total time elapsed: %f ms" total
    printfn ""

    0