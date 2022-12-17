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


let part1 () =
    let input = 
        readEmbeddedRaw "day15"
        |> Seq.map (split "Sensor at x=, y=: closest beacon is at x=, y=" >> Array.map int)
        |> Seq.map (fun a -> (a[0], a[1]), (a[2], a[3]))

    let sensorDistances = Seq.map (fun (s, b) -> s, dist s b) input
    let rowToTest = 2000000
    let ranges = sensorDistances |> Seq.choose (onRow rowToTest)
    let onLine = input |> Seq.collect (fun (s, b) -> [s;b]) |> Seq.distinct |> Seq.filter (snd >> (=) rowToTest) |> Seq.length

    ranges 
    |> Seq.map (fun (a, b) -> set [a..b])
    |> Seq.reduce Set.union
    |> Set.count
    |> fun total -> total - onLine

let part2 () =
    0