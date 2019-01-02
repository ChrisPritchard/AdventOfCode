open System
open System.IO

type Instruction = TurnOn | TurnOff | Toggle

[<EntryPoint>]
let main _ =

    let input = File.ReadAllLines "input.txt"

    let parseLine (s: string) = 
        let coords = s.Replace(" through ", ",").Replace("turn off ", "").Replace("turn on ", "").Replace("toggle ", "").Split([|','|])
        let func =
            if s.StartsWith "turn off" then TurnOff
            else if s.StartsWith "turn on" then TurnOn
            else Toggle
        coords |> Array.map Int32.Parse |> fun array -> func, array.[0], array.[1], array.[2], array.[3]
    
    let instructions = input |> Array.map parseLine

    let applies (x, y) (_, x1, y1, x2, y2) = x >= x1 && x <= x2 && y >= y1 && y <= y2

    let lights = [0..999999] |> List.map (fun i -> 
        let x = i / 1000
        let y = i % 1000
        instructions 
        |> Array.filter (applies (x, y)) 
        |> Array.map (fun (ins, _, _, _, _) -> ins))

    let part1apply instructions = 
        Array.fold (fun state -> 
            function TurnOn -> true | TurnOff -> false | Toggle -> not state) 
            false instructions 

    let part1 = lights |> List.filter part1apply |> List.length
    printfn "part 1: %i" part1

    let part2apply instructions = 
        Array.fold (fun state -> 
            function TurnOn -> state + 1 | TurnOff -> max 0 (state - 1) | Toggle -> state + 2) 
            0 instructions 

    let part2 = lights |> List.sumBy part2apply
    printfn "part 2: %i" part2

    0