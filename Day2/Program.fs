open System
open System.IO

[<EntryPoint>]
let main _ =
    let lines = 
        File.ReadAllLines "input.txt" 
        |> Array.map (Seq.groupBy id >> Seq.map (snd >> Seq.length) >> Set.ofSeq) 
        |> Array.toList
    let (twos, threes) = 
        lines 
        |> List.fold (fun (tw, th) set -> 
            (if Set.contains 2 set then tw + 1 else tw),
            (if Set.contains 2 set then th + 1 else th)) (0, 0)
    printfn "%i" (twos * threes)
    0
