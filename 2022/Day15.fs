module Day15

open Common

let dist (x1, y1) (x2, y2) =
    abs (x2 - x1) + abs (y2 - y1)

let part1 () =
    let input = 
        readEmbeddedRaw "day15"
        |> Seq.map (split "Sensor at x=, y=: closest beacon is at x=, y=" >> Array.map int)
        |> Seq.map (fun a -> (a[0], a[1]), (a[2], a[3]))

    let sensorDistances = Seq.map (fun (s, b) -> s, dist s b) input

    let rowToTest = 2000000

    let inRange = Seq.filter (fun ((_, y), d) -> y + d >= rowToTest && y - d <= rowToTest) sensorDistances

    let sorted = inRange |> Seq.collect (fun ((x, _), d) -> [x-d;x+d]) |> Seq.sort |> Array.ofSeq
    let onLine = input |> Seq.collect (fun (s, b) -> [s;b]) |> Seq.filter (snd >> (=) rowToTest) |> Set.ofSeq

    let rec counter acc x =
        if x > sorted[sorted.Length - 1] then acc
        else
            let p = x, rowToTest
            if Set.contains p onLine then counter acc (x + 1)
            else if inRange |> Seq.exists (fun (s, d) -> dist s p <= d) then counter (acc + 1) (x + 1)
            else counter acc (x + 1)
    counter 0 sorted[0]

let part2 () =
    0