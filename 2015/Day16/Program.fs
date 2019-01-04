open System.IO
open System

[<EntryPoint>]
let main _ =

    let input = File.ReadAllLines "input.txt"

    let gifter = [
        "children", 3
        "cats", 7
        "samoyeds", 2
        "pomeranians", 3
        "akitas", 0
        "vizslas", 0
        "goldfish", 5
        "trees", 3
        "cars", 2
        "perfumes", 1
    ]

    let parseSue (line: string) =
        let line = line.Split ([|' ';':';','|], StringSplitOptions.RemoveEmptyEntries)
        line.[1], [
            line.[2], int line.[3]
            line.[4], int line.[5]
            line.[6], int line.[7]
        ] |> Map.ofList

    let sueInfo = input |> Array.map parseSue

    let validateSue sue =
        let sue = snd sue
        gifter 
        |> List.exists (fun (aspect, aspectVal) -> 
            Map.containsKey aspect sue && Map.find aspect sue <> aspectVal)
        |> not

    let part1 = sueInfo |> Array.filter validateSue |> Array.head |> fst
    printfn "part 1: %s" part1

    let validateSue sue =
        let sue = snd sue
        gifter 
        |> List.exists (fun (aspect, aspectVal) -> 
            let test = 
                if aspect = "cats" || aspect = "trees" then fun sv -> sv <= aspectVal
                else if aspect = "pomeranians" || aspect = "goldfish" then fun sv -> sv >= aspectVal
                else fun sv -> sv <> aspectVal
            Map.containsKey aspect sue && test <| Map.find aspect sue)
        |> not

    let part2 = sueInfo |> Array.filter validateSue |> Array.head |> fst
    printfn "part 2: %s" part2

    0