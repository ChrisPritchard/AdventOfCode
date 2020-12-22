module Common

open System
open System.Diagnostics

let newline = Environment.NewLine

let splitOn (split: string) (s: string) =
    s.Split ([|split|], StringSplitOptions.RemoveEmptyEntries)

let split (chars: string) (s: string) = 
    s.Split (Seq.toArray chars, StringSplitOptions.RemoveEmptyEntries)

let splits (strings: seq<string>) (s: string) =
    s.Split (Seq.toArray strings, StringSplitOptions.RemoveEmptyEntries)

let asString (chars: seq<char>) =
    String(Seq.toArray chars)

let time func = 
    let timer = Stopwatch.StartNew();    
    let result = func ()
    timer.Stop()
    result, timer.ElapsedMilliseconds

let timeForDay day part func =
    let res, elapsed = time func
    match box res with
    | :? int64 as o ->
        printfn "day%d part%d: %d (elapsed: %d ms)" day part o elapsed
    | :? uint64 as o ->
        printfn "day%d part%d: %d (elapsed: %d ms)" day part o elapsed
    | _ ->
        printfn "day%d part%d: %A (elapsed: %d ms)" day part res elapsed
