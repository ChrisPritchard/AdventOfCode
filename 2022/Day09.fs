module Day09

open Common
open System

//   PPP 
//  PHHHP
//  PHTHP
//  PHHHP
//   PPP 

// for a difference from current position of the head
// the difference to move for the tail
let adjust = Map [
    (-1,-2), (-1,-1)
    (0,-2), (0,-1)
    (1,-2), (1,-1)

    (-2,-1), (-1,-1)
    (2,-1), (1,-1)

    (-2,0), (-1,0)
    (2,0), (1,0)

    (-2,1), (-1,1)
    (2,1), (1,1)

    (-1,2), (-1,1)
    (0,2), (0,1)
    (1,2), (1,1)
]

let dirMap = Map [
    "R", (1, 0)
    "L", (-1, 0)
    "U", (0, -1)
    "D", (0, 1)
]

let part1() =
    // state is visited locations, tail location and head location
    ((Set [ (0,0) ], ((0,0), (0,0))), readEmbeddedRaw "day09")
    ||> Seq.fold (fun (visited, (tail, head)) instruction -> 
        let (dx, dy), mag = 
            let p = split " " instruction
            dirMap[p[0]], Int32.Parse p[1]
        ((visited, (tail, head)), [1..mag])
        ||> Seq.fold (fun (visited, ((tx, ty), (hx, hy))) _ ->
            let nhx, nhy = hx + dx, hy + dy 
            match Map.tryFind (nhx - tx, nhy - ty) adjust with
            | None -> visited, ((tx, ty), (nhx, nhy))
            | Some (dtx, dty) ->
                let ntx, nty = tx + dtx, ty + dty
                Set.add (ntx, nty) visited, ((ntx, nty), (nhx, nhy)) ))
    |> fst |> Seq.length
    

let part2() =
    0