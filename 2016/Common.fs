module Common

open System

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