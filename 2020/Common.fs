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
    printfn $"day{day} part{part}: {res} (elapsed: {elapsed} ms)"