module Day10

open System.IO

let input = File.ReadAllLines "./inputs/day10.txt"

let processed () = 
    let raw = input |> Array.map int |> Array.sortBy id
    Array.concat 
        [| 
            [|0|] 
            raw
            [|raw.[Array.length raw - 1]+3|] 
        |]

let part1 () =
    processed ()
    |> Array.windowed 2
    |> Array.map (fun a -> a.[1] - a.[0])
    |> Array.groupBy id
    |> Array.map (fun (k, v) -> k, Array.length v)
    |> Map.ofArray
    |> fun m -> m.[1] * m.[3]

let part2 () =
    let processed = processed () |> List.ofArray
    let last = processed.Length - 1
    let mutable total = 1L
    for i = 0 to last do
        let start = processed.[i]
        let future = [i + 1..last] |> List.tryFind (fun j -> let diff = processed.[j] - start in diff = 2 || diff = 3)
        match future with
        | None -> ()
        | Some j ->
            let diff = j - i
            if diff = 3 then 
                total <- total * 4L
            elif diff = 2 then
                total <- total * 2L
    total        
