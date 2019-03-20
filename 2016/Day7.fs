(*
--- Day 7: Internet Protocol Version 7 ---

While snooping around the local network of EBHQ, you compile a list of IP addresses (they're IPv7, of course; IPv6 is much too limited). You'd like to figure out which IPs support TLS (transport-layer snooping).

An IP supports TLS if it has an Autonomous Bridge Bypass Annotation, or ABBA. An ABBA is any four-character sequence which consists of a pair of two different characters followed by the reverse of that pair, such as xyyx or abba. However, the IP also must not have an ABBA within any hypernet sequences, which are contained by square brackets.

For example:

    abba[mnop]qrst supports TLS (abba outside square brackets).
    abcd[bddb]xyyx does not support TLS (bddb is within square brackets, even though xyyx is outside square brackets).
    aaaa[qwer]tyui does not support TLS (aaaa is invalid; the interior characters must be different).
    ioxxoj[asdfgh]zxcvbn supports TLS (oxxo is outside square brackets, even though it's within a larger string).

How many IPs in your puzzle input support TLS?
*)

module Day7

open System
open System.IO

let input = File.ReadAllLines "Day7-input.txt"

let ABBA s = 
    true

let valid (line: string) = 
    let segments = line.Split([|'[';']'|], StringSplitOptions.RemoveEmptyEntries)
    let mutable normal, hypernet = false, false
    for i = 0 to segments.Length - 1 do
        if i % 2 = 0 then
            if not normal && ABBA segments.[i] then normal <- true
        else
            if not hypernet && ABBA segments.[i] then hypernet <- true
    normal && not hypernet

let part1 () = 
    input |> Array.filter valid |> Array.length