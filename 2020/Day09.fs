module Day09

open System.IO
open Common

let input = File.ReadAllLines "./inputs/day09.txt"

let processed () =
    input |> Array.map int64

let part1 () =
    let processed = processed ()
    let preamble = 25
    let rec finder soFar i =
        let n = processed.[i]
        if i < preamble then
            finder (Map.add (i % preamble) n soFar) (i + 1)
        else
            let anyValid = soFar |> Map.exists (fun _ o -> Map.exists (fun _ v -> v = (n - o)) soFar)
            if not anyValid then 
                n
            else 
                finder (Map.add (i % preamble) n soFar) (i + 1)
    finder Map.empty 0

let part2 () =
    let target = part1 ()
    let processed = processed ()
    let rec finder start i acc min max =
        let n = processed.[i]
        let res = n + acc
        if res > target then
            finder (start + 1) (start + 1) 0L -1L -1L
        else
            let min = if min = -1L || n < min then n else min
            let max = if max = -1L || n > max then n else max
            if res = target then 
                min + max
            else 
                finder start (i + 1) res min max
    finder 0 0 0L -1L -1L