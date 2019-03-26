
open System.Diagnostics

[<EntryPoint>]
let main _ =
    
    let time day part func =
        let timer = Stopwatch.StartNew();    
        let result = func ()
        timer.Stop()
        printfn "day%i part%i: %A (elapsed: %i ms)" day part result timer.ElapsedMilliseconds

    time 1 1 Day1.part1
    time 1 1 Day1.part2
    time 2 1 Day2.part1
    time 2 2 Day2.part2
    time 3 1 Day3.part1
    time 3 2 Day3.part2
    time 4 1 Day4.part1
    time 4 2 Day4.part2
    //time 5 1 Day5.part1 // 5-15 second approx run time
    //time 5 2 Day5.part2 // 8-30 second approx run time
    time 6 1 Day6.part1
    time 6 2 Day6.part2
    time 7 1 Day7.part1
    time 7 2 Day7.part2
    time 8 1 Day8.part1
    time 8 2 Day8.part2
    time 9 1 Day9.part1
    time 9 2 Day9.part2
    time 10 1 Day10.part1
    time 10 2 Day10.part2
    //time 11 1 Day11.part1 // 5 min+ approx run time
    //time 11 2 Day11.part2 // 5 min+ approx run time
    time 12 1 Day12.part1
    //time 12 2 Day12.part2 // 1.5 second+ approx run time
    time 13 1 Day13.part1
    time 13 2 Day13.part2
    time 14 1 Day14.part1
    //time 14 2 Day14.part2 // 1 min+ approx run time
    time 15 1 Day15.part1
    time 15 2 Day15.part2
    time 16 1 Day16.part1
    //time 16 2 Day16.part2 // 4 second+ approx run time
    time 17 1 Day17.part1
    //time 17 2 Day17.part2 // 1.5 second+ approx run time
    time 18 1 Day18.part1
    //time 18 2 Day18.part2 // 1.5 second+ approx run time
    time 19 1 Day19.part1
    time 19 2 Day19.part2
    time 20 1 Day20.part1
    time 20 2 Day20.part2
    time 21 1 Day21.part1
    //time 21 2 Day21.part2 // 7 second+ approx run time
    time 22 1 Day22.part1
    time 22 2 Day22.part2

    0
