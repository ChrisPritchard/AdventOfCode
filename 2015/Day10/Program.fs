
[<EntryPoint>]
let main _ =

    let input = "1321131112"
    let asList = input |> Seq.map (fun c -> int c - int '0') |> Seq.toList

    let processLine line =
        let soFar, last =
            line |> Seq.fold (fun (soFar, current) next ->
                match current with
                | None -> soFar, Some (next, 1)
                | Some (c, n) when c = next -> soFar, Some (c, n + 1)
                | Some (c, n) -> c::n::soFar, Some (next, 1)) ([], None)
        match last with
        | None -> List.rev soFar
        | Some (c, n) -> List.rev (c::n::soFar) 

    let part1 = [1..40] |> List.fold (fun state _ -> processLine state) asList
    printfn "part 1: %i" <| List.length part1

    let part2 = [1..10] |> List.fold (fun state _ -> processLine state) part1
    printfn "part 2: %i" <| List.length part2

    0