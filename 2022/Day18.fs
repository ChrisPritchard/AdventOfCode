module Day18

open Common

// coord system: x,y,z,facing, with the last being one of 6 values? only 3 are needed, and required to gen duplicates
// this could be represented using a 2 bits, then each of the others with 8 bits? or just use tuples

let part1 () =
    readEmbeddedRaw "day18"
    |> Seq.collect (fun line -> 
        let x, y, z = line |> split "," |> Array.map int |> fun a -> a[0], a[1], a[2]
        seq [
            x, y, z, 0 // up
            x, y, z-1, 0 // down
            x, y, z, 1 // north
            x, y+1, z, 1 // south
            x, y, z, 2 // west
            x+1, y, z, 2 // east
        ])
    |> Seq.countBy id |> Seq.filter (snd >> (=) 1) |> Seq.length

let part2 () =
    0