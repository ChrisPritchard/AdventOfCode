module Common

open System

let split (chars: string) (s: string) = 
    let chars = Seq.toArray chars
    s.Split (chars, StringSplitOptions.RemoveEmptyEntries)

let asString (chars: seq<char>) =
    String(Seq.toArray chars)