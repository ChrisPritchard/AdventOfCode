module Day02

open System.IO
open Common

let input = File.ReadAllLines "./inputs/day02.txt"

let processed () = 
    input 
    |> Array.map (fun s ->
        let parts = split "- :" s
        (int parts.[0], int parts.[1], char parts.[2], parts.[3]))

let part1 () =
    processed () 
    |> Array.filter (fun (n1, n2, c, s) ->
        let count = s |> Seq.filter ((=) c) |> Seq.length
        count >= n1 && count <= n2)
    |> Array.length

let part2 () =
    processed () 
    |> Array.filter (fun (n1, n2, c, s) ->
        let dict = s |> Array.ofSeq
        (dict.[n1 - 1] = c) <> (dict.[n2 - 1] = c))
    |> Array.length