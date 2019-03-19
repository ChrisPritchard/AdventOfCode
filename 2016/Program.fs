
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

    0
