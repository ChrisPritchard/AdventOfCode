
[<EntryPoint>]
let main _ =

    let input = 33100000

    let presents house =
        [1..house] |> List.sumBy (fun n -> 
            if house % n = 0 then n * 10 else 0)

    let rec find min max =
        let mid = (max - min) / 2 + min
        let presents = presents mid
        if presents = input then mid
        else if max - mid = 2 then max - 1
        else if presents < input then find mid max
        else find min mid

    let part1 = find 1000000 10000000
    printfn "part 1: %i" part1

    printfn "%i" <| presents 1561793

    0
