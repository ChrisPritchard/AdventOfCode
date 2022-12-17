module Day15

open Common

let dist (x1, y1) (x2, y2) =
    abs (x2 - x1) + abs (y2 - y1)

let onRow row ((sx, sy), d) =
    if sy <= row && sy + d >= row then
        let rd = row - sy // if sy is 4, row is 7, and d is 9, then the d for row 7 will be 9 - 3 = 6
        Some (sx - rd, sx + rd)
    else if sy >= row && sy - d <= row then
        let rd = sy - row
        Some (sx - rd, sx + rd)
    else None

let part1 () =
    let input = 
        readEmbeddedRaw "day15"
        |> Seq.map (split "Sensor at x=, y=: closest beacon is at x=, y=" >> Array.map int)
        |> Seq.map (fun a -> (a[0], a[1]), (a[2], a[3]))

    let sensorDistances = Seq.map (fun (s, b) -> s, dist s b) input
    let rowToTest = 2000000
    let ranges = sensorDistances |> Seq.choose (onRow rowToTest)
    let onLine = input |> Seq.collect (fun (s, b) -> [s;b]) |> Seq.filter (snd >> (=) rowToTest) |> Seq.length

    // take a set of ranges, sort by min
    // for each range, if its max is over the next min, reduce it to that min - 1. if this is negative then set its dist to 0
    // how to do this... windowed? for eac

    ranges
    |> Seq.append ([System.Int32.MaxValue, 0])
    |> Seq.sortBy fst
    |> Seq.windowed 2
    |> Seq.sumBy (fun pair ->
        let (min, max) = Seq.head pair
        let (omin, omax) = Seq.last pair
        // three scenarios: max is less than other (no overlap), max is more than other but less than other max (partial overlap), max is more than other max (total overlap)
        // if no overlap, add to total, if partial then add less than next, if total, add pre and post
        if max < min then max - min
        else if max > omax then (omin - min) + (max - omax)
        else omin - min)

let part2 () =
    0