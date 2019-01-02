
open System.IO
open System

[<EntryPoint>]
let main _ =

    let input = File.ReadAllLines "input.txt"

    let parseDims (s:string) = s.Split [|'x'|] |> Seq.map Int32.Parse |> Seq.sortBy id |> Seq.toList
    let paper (dims: int list) = (dims.[0] * dims.[1]) * 3 + (dims.[0] * dims.[2] * 2) + (dims.[1] * dims.[2] * 2)
    let ribbon (dims: int list) = (dims.[0] * 2) + (dims.[1] * 2) + (dims.[0] * dims.[1] * dims.[2])

    let totalPaper = input |> Seq.sumBy (parseDims >> paper)
    printfn "part 1: %i" totalPaper
    
    let totalRibbon = input |> Seq.sumBy (parseDims >> ribbon)
    printfn "part 2: %i" totalRibbon

    0
