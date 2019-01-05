open System.IO
open System
open System.Text.RegularExpressions

[<EntryPoint>]
let main _ =

    let input = File.ReadAllLines "input.txt"
    let molecule = Array.last input
    let mappings = 
        Array.truncate (input.Length - 2) input 
        |> Array.map (fun line -> 
            let line = line.Split [|' '|]
            line.[0], line.[2])

    let indexes text toMatch = 
        Regex.Matches (text, Regex.Escape toMatch) 
        |> Seq.map (fun r -> r.Index) |> Seq.toList

    let moleculesFrom (orig, repl) molecule =
        indexes molecule orig
        |> List.map (fun i ->
            molecule.[0..i-1] + repl + molecule.[i + orig.Length..])

    let allChanges = mappings |> Seq.collect (fun mapping -> moleculesFrom mapping molecule) |> Seq.distinct |> Seq.toList
    let part1 = List.length allChanges
    printfn "part 1: %i" part1

    // part 2 is based on the clever solution in the text file linked.
    // my brute force solution didnt work :)

    let elementCount = molecule |> Seq.filter Char.IsUpper |> Seq.length
    let count elem = indexes molecule elem |> List.length
    let part2 = elementCount - (count "Rn" + count "Ar") - (count "Y" * 2) - 1
    printfn "part 2: %i" part2

    0