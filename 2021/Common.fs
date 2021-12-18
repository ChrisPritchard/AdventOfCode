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
let readEmbedded file =
    use s: Stream = Assembly.GetEntryAssembly().GetManifestResourceStream ("2021." + file)
    use r = new StreamReader(s)
    r.ReadToEnd() |> split "\r\n"

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
