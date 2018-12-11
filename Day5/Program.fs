open System.IO

let toUpper = System.Char.ToUpper

let rec part1 polymer =
    let startLength = Seq.length polymer
    let result =
        polymer
        |> Seq.fold (fun soFar next ->
            match soFar with
            | last::tail ->
                if toUpper last = toUpper next && last <> next then tail
                else next::last::tail
            | [] -> [next]) []
    if List.length result = startLength then startLength
    else part1 (result |> Seq.ofList)

let part2 polymer =
    ['a'..'z'] 
    |> List.filter (fun letter -> Seq.contains letter polymer)
    |> List.map (fun letter -> 
        polymer |> Seq.filter (fun c -> c <> letter && c <> toUpper letter) |> part1)
    |> List.minBy id

[<EntryPoint>]
let main _ =
    
    let polymer = File.ReadAllText "input.txt"
    //let polymer = "dabAcCaCBAcCcaDA"

    printfn "part1: %i" <| part1 polymer
    printfn "part2: %i" <| part2 polymer

    0