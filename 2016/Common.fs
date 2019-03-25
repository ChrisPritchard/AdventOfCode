module Common

open System
open System.Security.Cryptography

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

let lines s =
    split "\r\n" s

let md5 = MD5.Create ()

let hexMd5Hash (s: string) = 
    let bytes = (s.ToCharArray ()) |> Array.map byte
    let hash = md5.ComputeHash (bytes) |> Array.map (fun b -> Convert.ToString(b, 16).PadLeft(2, '0'))
    (String.concat "" hash).ToLower()