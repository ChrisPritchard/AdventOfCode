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
    map, (500-minx + 1, 0)

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

// flowing out rules
// if map is sand make falling
// if space beneath is sand then return drop and end
// if map is clay stop with current

let flowOut (map: Tile [,]) (x, y) =
    let rec flow ox dx soFar = 
        if ox < 0 || ox = Array2D.length1 map || map.[ox, y] = Clay then soFar, None
        else
            map.[ox, y] <- FallingWater
            if map.[ox, y + 1] = Sand then
                (ox, y)::soFar, Some (ox, y + 1)
            else
                flow (ox + dx) dx ((ox, y)::soFar)
    let tilesLeft, leftDrop = flow x -1 []
    let tilesRight, rightDrop = flow x 1 []
    tilesLeft @ [x, y] @ tilesRight, leftDrop, rightDrop

let rec drop (map: Tile [,]) (x, y) =
    match Array2D.length2 map - y with
    | 0 -> ()
    | 1 ->
        map.[x, y] <- FallingWater
    | _ ->
        map.[x, y] <- FallingWater
        match map.[x, y + 1] with
        | Sand -> drop map (x, y + 1)
        | _ ->
            let tiles, leftDrop, rightDrop = flowOut map (x, y)
            match leftDrop, rightDrop with
            | None, None ->
                tiles |> List.iter (fun (tx, ty) -> map.[tx, ty] <- RestingWater)
                if y <> 0 then 
                    drop map (x, y - 1)
            | Some single, None | None, Some single -> drop map single
            | Some left, Some right -> 
                drop map left
                drop map right

// 1. mark as falling
// 2. check next down
// 3. if sand start again on next down
// 4. else expand out
// 5. if contained, change row to resting and start again one up
// 6. while expanding, if sound found down stop expanding (in this direction) and start again falling

[<EntryPoint>]
let main _ =

    let input = File.ReadAllLines "input.txt"

    let clay = parseInput input
    let bounds = boundsByClay clay

    let map, (sx, sy) = mapFromClay clay bounds
    System.IO.File.WriteAllLines ("start.txt", renderMap map)
    drop map (sx, sy + 1)
    System.IO.File.WriteAllLines ("out.txt", renderMap map)
    printfn "part 1: %i" <| water map

    0
