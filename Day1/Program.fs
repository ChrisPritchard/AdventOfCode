
open System
open System.IO



let part1 file = 
    File.ReadAllLines file
    |> Seq.fold (fun n line -> 
        let num = Int32.Parse(line)
        n + num) 0

let part2 file = 

    let rec findDup state found remaining =
        let head = Seq.head remaining
        let next = state + head
        if Set.contains next found then next
        else findDup next (Set.add next found) (Seq.tail remaining)

    let lines = File.ReadAllLines file |> Array.map (Int32.Parse)
    Seq.initInfinite (fun _ -> lines) 
    |> Seq.collect id
    |> findDup 0 (Set.empty.Add(0))

[<EntryPoint>]
let main _ =
    printfn "%i" <| part1 "input.txt"
    printfn "%i" <| part2 "input.txt"
    0