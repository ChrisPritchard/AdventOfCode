open System
open System.IO
open FParsec.CharParsers
open FParsec

let input () =
    let lines = File.ReadAllLines "input.txt"

    let pline = pint32 .>> pstring ", " .>>. pint32
    let processLine line =
        match run pline line with
        | Success (result, _, _) -> result
        | Failure (error, _, _) -> failwith error

    lines |> Seq.map processLine |> Seq.toList

let manhatten (x,y) (ox, oy) = abs (x - ox) + abs (y - oy)

let part1 points (minx, miny, maxx, maxy) =
    let invalid (x, y) = x <= minx || x >= maxx || y <= miny || y >= maxy

    [minx..maxx] |> List.collect (fun x ->
    [miny..maxy] |> List.map (fun y ->
        let coord = x, y
        let dists = points |> List.map (fun p -> p, manhatten p coord) |> List.sortBy snd
        match dists with
        | (_, dist1)::(_, dist2)::_ when dist1 = dist2 -> None
        | (p1, _)::_ -> Some (coord, p1)
        | _ -> None))
        |> List.choose id
        |> List.groupBy snd
        |> List.map (fun (point, list) -> point, List.map fst list)
        |> List.filter (fun (_, list) -> List.tryFind invalid list = None)
        |> List.maxBy (fun (_, list) -> List.length list)
        |> snd |> List.length

let part2 points (minx, miny, maxx, maxy) =
    [minx..maxx] |> List.collect (fun x ->
    [miny..maxy] |> List.filter (fun y ->
        let coord = x, y
        let dists = points |> List.sumBy (fun p -> manhatten p coord)
        dists < 10000))
    |> List.length

[<EntryPoint>]
let main _ =

    let points = input ()    
    let bounds =
        points |> List.fold (fun (minx, miny, maxx, maxy) (x, y) ->
            (if x < minx then x else minx),
            (if y < miny then y else miny),
            (if x > maxx then x else maxx),
            (if y > maxy then y else maxy)) (System.Int32.MaxValue, System.Int32.MaxValue, 0, 0)

    printfn "part 1: %i" <| part1 points bounds
    printfn "part 2: %i" <| part2 points bounds

    0