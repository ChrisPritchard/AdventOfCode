open System
open System.IO
open FParsec.CharParsers
open FParsec

[<EntryPoint>]
let main _ =

    // let lines = File.ReadAllLines "input.txt"
    // // let lines = [|
    // //     "1, 1"
    // //     "1, 6"
    // //     "8, 3"
    // //     "3, 4"
    // //     "5, 5"
    // //     "8, 9"
    // // |]

    // let pline = pint32 .>> pstring ", " .>>. pint32
    // let processLine line =
    //     match run pline line with
    //     | Success (result, _, _) -> result
    //     | Failure (error, _, _) -> failwith error

    // let points = lines |> Seq.map processLine |> Seq.toList

    // let points = [
    //     1, 1
    //     1, 6
    //     8, 3
    //     3, 4
    //     5, 5
    //     8, 9
    // ]

    let points = [
        342, 203
        79, 64
        268, 323
        239, 131
        246, 87
        161, 93
        306, 146
        43, 146
        57, 112
        241, 277
        304, 303
        143, 235
        253, 318
        97, 103
        200, 250
        67, 207
        345, 149
        133, 222
        232, 123
        156, 359
        80, 224
        51, 145
        138, 312
        339, 294
        297, 256
        163, 311
        241, 321
        126, 66
        145, 171
        359, 184
        241, 58
        108, 312
        117, 118
        101, 180
        58, 290
        324, 42
        141, 190
        270, 149
        209, 294
        296, 345
        68, 266
        233, 281
        305, 183
        245, 230
        161, 295
        335, 352
        93, 66
        227, 59
        264, 249
        116, 173
    ]

    let minx, miny, maxx, maxy =
        points |> List.fold (fun (minx, miny, maxx, maxy) (x, y) ->
            (if x < minx then x else minx),
            (if y < miny then y else miny),
            (if x > maxx then x else maxx),
            (if y > maxy then y else maxy)) (System.Int32.MaxValue, System.Int32.MaxValue, 0, 0)

    let noninfinite = points |> List.filter (fun (x, y) -> x <> minx && x <> maxx && y <> miny && y <> maxy)
    let manhatten (x,y) (ox, oy) = abs (x - ox) + abs (y - oy)

    let candidates =
        [minx..maxx] |> List.collect (fun x ->
        [miny..maxy] |> List.map (fun y ->
            let coord = x, y
            let dists = points |> List.map (fun p -> p, manhatten p coord) |> List.sortBy snd
            match dists with
            | (_, dist1)::(_, dist2)::_ when dist1 = dist2 -> None
            | (p1, _)::_ when noninfinite |> Seq.contains p1 -> Some (coord, p1)
            | _ -> None))
            |> List.choose id
            |> Map.ofList

    let named = points |> List.mapi (fun i p -> p, sprintf "%2i" i) |> Map.ofList

    let text = 
        [miny..maxy] |> List.collect (fun y ->
        "\r\n"::
            ([miny..maxy] |> List.map (fun x -> 
            match named.TryFind (x, y) with
            | Some _ -> "XX"
            | _ ->
                match candidates.TryFind (x, y) with
                | Some n -> named.[n]
                | _ -> "__")))
        |> String.Concat
    System.IO.File.WriteAllText("test.txt", text)


    // let part1 = 
    //     candidates 
    //     |> List.choose id
    //     |> List.groupBy id
    //     |> List.map (snd >> List.length)
    //     |> List.max

    // printfn "part 1: %i" part1

    0