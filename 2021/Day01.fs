module Day01

open System.Reflection
open System.IO
open Common

let input = 
    use s: Stream = Assembly.GetEntryAssembly().GetManifestResourceStream "2021.day01"
    use r = new StreamReader(s)
    r.ReadToEnd() |> split "\r\n"

let processed () = Array.map int input

let part1 () =
    processed () 
    |> Array.windowed 2
    |> Array.filter (fun set -> set[1] > set[0])
    |> Array.length

let part2 () =
    processed () 
    |> Array.windowed 3
    |> Array.map Array.sum
    |> Array.windowed 2
    |> Array.filter (fun set -> set[1] > set[0])
    |> Array.length