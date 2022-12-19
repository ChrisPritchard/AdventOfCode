module Day18

open Common

let faces (x, y, z) =
    seq [
            x, y, z, 0 // up
            x, y, z-1, 0 // down
            x, y, z, 1 // north
            x, y+1, z, 1 // south
            x, y, z, 2 // west
            x+1, y, z, 2 // east
        ]

let part1 () =
    readEmbeddedRaw "day18"
    |> Seq.collect (fun line -> line |> split "," |> Array.map int |> (fun a -> a[0], a[1], a[2]) |> faces)
    |> Seq.countBy id |> Seq.filter (snd >> (=) 1) |> Seq.length

let part2 () =

    let cubes = 
        readEmbeddedRaw "day18"
        |> Seq.map (fun line -> line |> split "," |> Array.map int |> fun a -> a[0], a[1], a[2])
        |> Set.ofSeq

    let rec flood water queue =
        match queue with
        | [] -> water
        | (x, y, z)::rem ->
            let water = Set.add (x, y, z) water
            let next = 
                seq [
                    -1,0,0
                    1,0,0
                    0,-1,0
                    0,1,0
                    0,0,-1
                    0,0,1
                ] 
                |> Seq.map (fun (dx, dy, dz) -> x + dx, y + dy, z + dz)
                |> Seq.filter (fun (x, y, z) -> 
                    x >= -1 && y >= -1 && z >= -1 &&
                        x <= 22 && y <= 22 && z <= 22 &&
                            not (Set.contains (x, y, z) cubes) &&
                            not (Set.contains (x, y, z) water))
                |> Seq.toList
            flood water (List.append next rem)

    let water = 
        flood Set.empty [-1,-1,-1]
        |> Seq.collect faces |> Seq.distinct |> Set.ofSeq

    cubes 
    |> Seq.collect faces 
    |> Seq.countBy id 
    |> Seq.choose (fun (face, count) -> if count = 1 then Some face else None) 
    |> Seq.filter (fun face -> Set.contains face water) 
    |> Seq.length

