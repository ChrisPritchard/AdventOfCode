open System

let geologicIndex p target erosionLevels =
    match p with
    | 0, 0 -> 0
    | _ when p = target -> 0
    | x, y when y = 0 -> x * 16807
    | x, y when x = 0 -> y * 48271
    | x, y -> 
        Map.find (x-1, y) erosionLevels 
        * Map.find (x, y-1) erosionLevels

[<EntryPoint>]
let main argv =
    let depth = 10914
    let tx, ty = 9,739

    let riskLevel =
        [0..tx] |> List.collect (fun x ->
        [0..ty] |> List.map (fun y -> x, y))
        |> List.fold (fun erosionLevels p -> 
            let g = geologicIndex p (tx, ty) erosionLevels
            let e = (g + depth) % 20183
            Map.add p e erosionLevels) Map.empty
        |> Map.toList |> List.sumBy (snd >> fun e -> e % 3)

    printfn "part 1: %i" riskLevel

    0
