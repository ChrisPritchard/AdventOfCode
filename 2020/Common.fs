module Common

open System
open System.Security.Cryptography
open System.Diagnostics

let splitOn (split: string) (s: string) =
    s.Split ([|split|], StringSplitOptions.RemoveEmptyEntries)

let split (chars: string) (s: string) = 
    s.Split (Seq.toArray chars, StringSplitOptions.RemoveEmptyEntries)

let splits (strings: seq<string>) (s: string) =
    s.Split (Seq.toArray strings, StringSplitOptions.RemoveEmptyEntries)

let asString (chars: seq<char>) =
    String(Seq.toArray chars)

let contains (t: string) (s: string) =
    s.Contains t

let index (t:string) (s:string) =
    s.IndexOf t

let replace (t:string) (o: string) (s:string) =
    s.Replace (t, o)

let lines s =
    split "\r\n" s

let md5 = MD5.Create ()

let hex (bytes: seq<byte>) =
    bytes
    |> Seq.map (fun b -> Convert.ToString(b, 16).PadLeft(2, '0'))
    |> String.concat ""

let hexMd5Hash (s: string) = 
    let bytes = (s.ToCharArray ()) |> Array.map byte
    let hash = md5.ComputeHash (bytes) |> Array.map (fun b -> Convert.ToString(b, 16).PadLeft(2, '0'))
    (String.concat "" hash).ToLower()

let dist2 (x1, y1) (x2, y2) =
    sqrt ((x2 - x1) ** 2. + (y2 - y1) ** 2.)

let dist3 (x1, y1, z1) (x2, y2, z2) =
    sqrt ((x2 - x1) ** 2. + (y2 - y1) ** 2. + (z2 - z1) ** 2.)

let time func = 
    let timer = Stopwatch.StartNew();    
    let result = func ()
    timer.Stop()
    result, timer.ElapsedMilliseconds

let timeForDay day part func =
    let res, elapsed = time func
    printfn "day%i part%i: %A (elapsed: %i ms)" day part res elapsed

let newline = Environment.NewLine