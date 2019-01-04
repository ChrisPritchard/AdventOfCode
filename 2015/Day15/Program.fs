
[<EntryPoint>]
let main _ =

    let input = [
        [4;-2;0;0;5]
        [0;5;-1;0;8]
        [-1;0;5;0;6]
        [0;0;-2;2;1]
    ]

    let combinations = 
        [0..100] |> List.collect (fun frosting ->
        [0..100-frosting] |> List.collect (fun candy ->
        [0..100-candy-frosting] |> List.collect (fun butterscotch ->
        [0..100-butterscotch-candy-frosting] |> List.map (fun sugar ->
            [0..4] |> List.map (fun i -> 
               max 0 ((frosting * input.[0].[i])
               + (candy * input.[1].[i])
               + (butterscotch * input.[2].[i])
               + (sugar * input.[3].[i])))))))

    let part1 = 
        combinations 
        |> List.map (List.take 4 >> List.fold (*) 1) 
        |> List.max
    printfn "part 1: %i" part1

    0
