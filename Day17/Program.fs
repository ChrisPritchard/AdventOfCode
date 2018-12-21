open System
open System.IO
open FParsec.CharParsers
open FParsec

type Tile = Sand | Clay | FallingWater | RestingWater | Spring

let parseInput lines = 
    let pcoord = anyOf ['x';'y'] .>> pchar '=' .>>. pint32
    let pstart = pcoord .>> pstring ", "
    let pend = pcoord .>> pstring ".." .>>. pint32
    let pline = pstart .>>. pend |>> fun ((c, s1), ((_, s2), e)) ->
        if c = 'x' then
            (s1,s1), (s2,e)
        else
            (s2,e), (s1,s1)
    let parseLine = run pline >> function | Success (r, _, _) -> r | Failure (e, _, _) -> failwith e
    lines |> Seq.map parseLine |> Seq.toList

let boundsByClay =
    List.fold (fun (miny, maxy, minx, maxx) ((mix, mxx), (miy, mxy)) -> 
        (min miy miny), (max mxy maxy), 
        (min mix minx), (max mxx maxx)) 
        (Int32.MaxValue, 0, Int32.MaxValue, 0)

let mapFromClay clay (miny, maxy, minx, maxx) = 
    let map = Array2D.create ((maxx - minx) + 3) ((maxy - miny) + 2) Sand
    map.[500-minx + 1, 0] <- Spring
    let updateMap ((x,w), (y,h)) =
        [x..w] |> List.iter (fun x ->
        [y..h] |> List.iter (fun y -> map.[1 + x-minx,1 + y-miny] <- Clay))
    clay |> List.iter updateMap
    map

let renderMap (map: Tile [,]) = 
    [0..Array2D.length2 map - 1] |> List.map (fun y ->
        [0..Array2D.length1 map - 1] 
        |> List.map (fun x -> 
            match map.[x, y] with
            | Sand -> "."
            | Clay -> "#"
            | FallingWater -> "|"
            | RestingWater -> "~"
            | Spring -> "+")
        |> String.concat "")

let water map = 
    [0..Array2D.length2 map - 1] |> List.sumBy (fun y ->
        [0..Array2D.length1 map - 1] 
        |> List.sumBy (fun x -> 
            match map.[x, y] with
            | Sand | Clay -> 0
            | FallingWater | RestingWater -> 1
            | Spring -> 0))

[<EntryPoint>]
let main _ =

    let input = File.ReadAllLines "input.txt"

    let clay = parseInput input
    let bounds = boundsByClay clay

    let map = mapFromClay clay bounds
    renderMap map |> List.iter (printfn "%s")

    0
