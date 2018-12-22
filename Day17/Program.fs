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

let spaceFree map (x, y) =
    let maxWidth = Array2D.length1 map
    let check found dx =
        match found with
        | Some _ -> found
        | None ->
            match map.[x + dx, y] with
            | Sand -> Some <| Some (x + dx)
            | Clay | FallingWater -> Some None
            | _ -> None
    match [-1..-1..-x] |> List.fold check None with
    | Some (Some ox) -> Some (ox, y)
    | _ -> 
        match [1..(maxWidth-x)] |> List.fold check None with
        | Some (Some ox) -> Some (ox, y)
        | _ -> None

let contained map (x, y) = 
    let maxWidth = Array2D.length1 map
    let check soFar dx =
        match soFar with
        | Some _ -> soFar
        | None ->
            match map.[x + dx, y] with
            | Clay -> Some true
            | _ -> 
                match map.[x + dx, y + 1] with
                | Clay | RestingWater -> None
                | _ -> Some false
    match [-1..-1..-x] |> List.fold check None, [1..(maxWidth-x)] |> List.fold check None with
    | Some true, Some true -> true
    | _ -> false

let overflow map (x, y) next =
    let maxWidth = Array2D.length1 map
    let check soFar dx =
        if soFar then 
            match map.[x + dx, y] with
            | Sand | FallingWater ->
                map.[x + dx, y] <- FallingWater
                match map.[x + dx, y + 1] with
                | Sand | FallingWater -> 
                    map.[x + dx, y + 1] <- FallingWater
                    next (x + dx, y + 1)
                    false
                | _ -> true
            | _ -> false
        else false
    [-1..-1..-x] |> List.fold check true |> ignore
    [1..(maxWidth-x)] |> List.fold check true |> ignore

let rec drop map (x, y) = 
    if y = (Array2D.length2 map - 1) then ()
    else
        match map.[x, y + 1] with
        | FallingWater -> drop map (x, y + 1)
        | Clay -> map.[x, y] <- RestingWater
        | Sand -> 
            map.[x, y + 1] <- FallingWater
            drop map (x, y + 1)
        | RestingWater ->
            match spaceFree map (x, y + 1) with
            | Some (ox, oy) -> map.[ox, oy] <- RestingWater
            | None ->
                if contained map (x, y) 
                then map.[x, y] <- RestingWater
                else overflow map (x, y) (drop map)
        | Spring -> ()

let rec part1 map spring last1 last2 =
    drop map spring
    let count = water map
    if count = last1 && count = last2 then count
    else part1 map spring count last1

[<EntryPoint>]
let main _ =

    let input = File.ReadAllLines "input.txt"

    let clay = parseInput input
    let bounds = boundsByClay clay

    let map, spring = mapFromClay clay bounds
    [1..200] |> List.iter (fun _ -> drop map spring)
    System.IO.File.WriteAllLines ("out.txt", renderMap map)
    //printfn "part 1: %i" <| part1 map spring 0 0 

    0
