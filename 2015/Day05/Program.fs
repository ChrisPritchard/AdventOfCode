open System.IO

[<EntryPoint>]
let main _ =
    
    let input = File.ReadAllLines "input.txt"

    let naughtyPairs = ["ab";"cd";"pq";"xy"] |> List.map (fun s -> [|s.[0];s.[1]|])
    let vowels = "aeiou"

    let check s =
        let windows = Seq.windowed 2 s |> Seq.toList
        if windows |> List.filter (fun pair -> List.contains pair naughtyPairs |> not) |> List.length <> List.length windows then false
        else if windows |> List.exists (fun window -> window.[0] = window.[1]) |> not then false
        else
            let vowels = s |> Seq.filter (fun c -> vowels.Contains c) |> Seq.length
            vowels >= 3
    
    let part1 = input |> Seq.filter check |> Seq.length
    printfn "part 1: %i" part1

    let check s =
        let windows = Seq.windowed 2 s |> Seq.mapi (fun i w -> i, w) |> Seq.toList
        let pair = 
            windows 
            |> List.exists (fun (index, window) -> 
                index < windows.Length - 2 && 
                List.exists (fun (_, g) -> g = window) windows.[index + 2..])
        pair &&
            Seq.windowed 3 s |> Seq.exists (fun window -> window.[0] = window.[2] && window.[0] <> window.[1])

    let part2 = input |> Seq.filter check |> Seq.length
    printfn "part 2: %i" part2

    0
