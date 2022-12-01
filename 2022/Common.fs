module Common

open System
open System.IO
open System.Reflection
open System.Diagnostics

let splitOn (split: string) (s: string) =
    s.Split ([|split|], StringSplitOptions.RemoveEmptyEntries)

let split (chars: string) (s: string) = 
    s.Split (Seq.toArray chars, StringSplitOptions.RemoveEmptyEntries)

let splits (strings: seq<string>) (s: string) =
    s.Split (Seq.toArray strings, StringSplitOptions.RemoveEmptyEntries)

let asString (chars: seq<char>) =
    String(Seq.toArray chars)

let asInt (c: char) = int c - int '0'

// expects files to be in the format 2021.[filename]
let readEmbeddedRaw file = seq {
        use s: Stream = Assembly.GetEntryAssembly().GetManifestResourceStream ("2022." + file)
        use r = new StreamReader(s)
        while not r.EndOfStream do
            yield r.ReadLine ()
    }

// expects files to be in the format 2021.[filename]
let readEmbedded file =
    readEmbeddedRaw file |> Seq.toArray

let time func = 
    let timer = Stopwatch.StartNew();    
    let result = func ()
    timer.Stop()
    let milliseconds = (float timer.ElapsedTicks) / (float Stopwatch.Frequency)
    result, milliseconds*1000.0

let timeForDay day part func =
    let res, elapsed = time func
    match box res with
    | :? int64 as o ->
        printfn "day%d part%d: %d (elapsed: %f ms)" day part o elapsed
    | :? uint64 as o ->
        printfn "day%d part%d: %d (elapsed: %f ms)" day part o elapsed
    | :? string as o ->
        printfn "day%d part%d: %s (elapsed: %f ms)" day part o elapsed
    | _ ->
        printfn "day%d part%d: %A (elapsed: %f ms)" day part res elapsed
    elapsed

let totalTimeForDay day func =
    let (part1, part2), elapsed = time func
    let part1Res = 
        match box part1 with
        | :? int64 as o ->
            sprintf "%d" o
        | :? uint64 as o ->
            sprintf "%d" o
        | :? string as o ->
            sprintf "%s" o
        | _ ->
            sprintf "%A" part1
    let part2Res = 
        match box part2 with
        | :? int64 as o ->
            sprintf "%d" o
        | :? uint64 as o ->
            sprintf "%d" o
        | :? string as o ->
            sprintf "%s" o
        | _ ->
            sprintf "%A" part2
    printfn "day%d part1: %s" day part1Res
    printfn "day%d part2: %s" day part2Res
    printfn "elapsed: %f ms" elapsed
    elapsed