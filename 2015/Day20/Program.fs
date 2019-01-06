
[<EntryPoint>]
let main _ =

    let input = 33100000

    let presents house =
        [1..(house |> float |> sqrt |> int)] 
        |> Seq.filter (fun n -> house % n = 0)
        |> Seq.collect (fun n -> [n; house / n])
        |> Seq.distinct
        |> Seq.sumBy (fun n -> n * 10)

    let part1 = [2..3000000] |> List.find (presents >> (<=) input)
    printfn "part 1: %i" part1

    0
