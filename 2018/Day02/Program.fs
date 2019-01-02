open System
open System.IO

let part1 lines =
    let grouped =
        lines |> List.map (Seq.groupBy id >> Seq.map (snd >> Seq.length) >> Set.ofSeq) 
    let (twos, threes) = 
        grouped 
        |> List.fold (fun (tw, th) set -> 
            (if Set.contains 2 set then tw + 1 else tw),
            (if Set.contains 3 set then th + 1 else th)) (0, 0)
    twos * threes

let part2 lines =
    let check (line1 : string, line2 : string) =
        let len = Seq.length line1
        if len <> Seq.length line2 then None
        else
            let result =
                Seq.zip line1 line2 
                |> Seq.filter (fun (a, b) -> a = b)
                |> Seq.map fst
                |> String.Concat
            if result.Length <> len-1 then None
            else Some result
    lines 
        |> List.collect (fun line -> 
            lines 
            |> List.map (fun oline -> line, oline))
        |> List.map check
        |> List.pick id

[<EntryPoint>]
let main _ =
    let rawLines = File.ReadAllLines "input.txt" |> Array.toList
    printfn "Part 1: %i" <| part1 rawLines
    printfn "Part 2: %s" <| part2 rawLines
    0
