
open System.Diagnostics

[<EntryPoint>]
let main _ =
    
    let timer = Stopwatch.StartNew();

    printfn "day1 part1: %A" <| Day1.part1 ()
    printfn "day1 part2: %A" <| Day1.part2 ()

    timer.Stop ()
    printfn "elapsed: %i ms" timer.ElapsedMilliseconds

    0
