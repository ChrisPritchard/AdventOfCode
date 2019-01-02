open System.IO

[<EntryPoint>]
let main _ =

    let input = File.ReadAllText "input.txt"
    
    let move (x, y) = 
        function
        | '^' -> x, y - 1
        | '>' -> x + 1, y
        | 'v' -> x, y + 1
        | _ -> x - 1, y

    let houses = 
        input 
        |> Seq.fold (fun soFar next -> 
            (move (List.head soFar) next)::soFar) [0, 0]

    let part1 = List.distinct houses |> List.length
    printfn "part 1: %i" part1

    let santaHouses, robotHouses, _ = 
        input
        |> Seq.fold (fun (santaSoFar, robotSoFar, index) next ->
            if index % 2 = 0 then
                (move (List.head santaSoFar) next)::santaSoFar, robotSoFar, index + 1
            else 
                santaSoFar, (move (List.head robotSoFar) next)::robotSoFar, index + 1) ([0,0], [0,0], 0)
    
    let part2 = santaHouses @ robotHouses |> List.distinct |> List.length
    printfn "part 2: %i" part2

    0
