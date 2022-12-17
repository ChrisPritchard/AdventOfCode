module Day15

open Common

let dist (x1, y1) (x2, y2) =
    abs (x2 - x1) + abs (y2 - y1)

let onRow row ((sx, sy), d) =
    if sy <= row && sy + d >= row then
        let rd = d - (row - sy) // if sy is 4, row is 7, and d is 9, then the d for row 7 will be 9 - 3 = 6
        Some (sx - rd, sx + rd)
    else if sy >= row && sy - d <= row then
        let rd = d - (sy - row)
        Some (sx - rd, sx + rd)
    else None

let subtract (min1, max1) (min2, max2) =
    if max1 < min2 || min1 > max2 then [min1, max1] // no overlap
    else if min1 < min2 && max1 > max2 then [min1,min2-1;max2+1,max1] // full overlap
    else if max1 = min2 || (min1 < min2 && max1 < max2) then [min1, min2-1] // overlap left
    else if min1 = max2 || (min1 > min2 && max1 > max2) then [max2+1, max1] // overlap right
    else [] // overlapped


let part1 () =
    let input = 
        readEmbeddedRaw "day15"
        |> Seq.map (split "Sensor at x=, y=: closest beacon is at x=, y=" >> Array.map int)
        |> Seq.map (fun a -> (a[0], a[1]), (a[2], a[3]))

    let sensorDistances = Seq.map (fun (s, b) -> s, dist s b) input
    let rowToTest = 2000000
    let ranges = sensorDistances |> Seq.choose (onRow rowToTest) |> List.ofSeq

    let onLine = input |> Seq.collect (fun (s, b) -> [s;b]) |> Seq.filter (snd >> (=) rowToTest) |> Seq.distinct |> Seq.length

    let rec reduce acc =
        function 
        | [] -> List.collect id acc
        | (min, max)::ranges when min = max -> reduce acc ranges
        | range::ranges ->
            let newranges = 
                ([range], ranges) 
                ||> List.fold (fun acc r2 -> acc |> List.collect (fun s -> subtract s r2))
            reduce (newranges::acc) ranges

    reduce [] ranges
    |> Seq.sumBy (fun (min, max) -> (max - min) + 1)
    |> fun total -> total - onLine

let part2 () =
    let input = 
        readEmbeddedRaw "day15"
        |> Seq.map (split "Sensor at x=, y=: closest beacon is at x=, y=" >> Array.map int)
        |> Seq.map (fun a -> (a[0], a[1]), (a[2], a[3]))

    let sensorDistances = Seq.map (fun (s, b) -> s, dist s b) input
    let max = 4000000

    let rec findBeacon line = 
        let ranges = sensorDistances |> Seq.choose (onRow line) |> List.ofSeq
        let free =
            ([0, max], ranges) 
            ||> List.fold (fun acc r2 -> 
                acc |> List.collect (fun s -> subtract s r2))
        if List.isEmpty free then findBeacon (line + 1)
        else if List.length free > 1 then failwith "expect only a single value"
        else int64 (fst (List.head free)) * int64 max + int64 line
    
    findBeacon 0