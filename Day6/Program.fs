open System
open System.IO
open FParsec.CharParsers
open FParsec

[<EntryPoint>]
let main _ =
    
    let lines = File.ReadAllLines "input.txt"
    // let lines = [|
    //     "1, 1"
    //     "1, 6"
    //     "8, 3"
    //     "3, 4"
    //     "5, 5"
    //     "8, 9"
    // |]

    let pline = pint32 .>> pstring ", " .>>. pint32
    let processLine line =
        match run pline line with
        | Success (result, _, _) -> result
        | Failure (error, _, _) -> failwith error

    let points = lines |> Seq.map processLine |> Seq.toList

    // let points = [
    //     1, 1
    //     1, 6
    //     8, 3
    //     3, 4
    //     5, 5
    //     8, 9
    // ]

    let minx, miny, maxx, maxy =
        points |> List.fold (fun (minx, miny, maxx, maxy) (x, y) ->
            (if x < minx then x else minx),
            (if y < miny then y else miny),
            (if x > maxx then x else maxx),
            (if y > maxy then y else maxy)) (System.Int32.MaxValue, System.Int32.MaxValue, 0, 0)
    
    let manhatten (x,y) (ox, oy) = abs (x - ox) + abs (y - oy)
    let noninfinite = points |> List.filter (fun (x, y) -> x <> minx && x <> maxx && y <> miny && y <> maxy)

    let part1 =
        [minx..maxx] |> List.collect (fun x -> 
        [miny..maxy] |> List.map (fun y ->
            let closest =
                points 
                |> List.map (fun p -> p, manhatten (x, y) p)
                |> List.sortBy snd
                |> List.take 2
            if closest.[0] = closest.[1] then None
            else if not <| Seq.contains (fst closest.[0]) noninfinite then None
            else Some closest.[0]))
            |> List.choose id
            |> List.groupBy fst
            |> List.maxBy (snd >> List.length)
            |> snd |> List.length

    printfn "part 1: %i" part1

    0