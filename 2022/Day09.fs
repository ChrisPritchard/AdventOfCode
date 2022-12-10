module Day09

open Common
open System

//  XPPPX
//  PHHHP
//  PHTHP
//  PHHHP
//  XPPPX

// for a difference from current position of the head
// the difference to move for the tail
let adjust = Map [
    (-1,-2), (-1,-1)    // top three P positions
    (0,-2), (0,-1)
    (1,-2), (1,-1)

    (-2,-1), (-1,-1) // two under top
    (2,-1), (1,-1)

    (-2,0), (-1,0) // two either side of middle
    (2,0), (1,0)

    (-2,1), (-1,1) // two above bottom
    (2,1), (1,1)

    (-1,2), (-1,1) // final three p positions
    (0,2), (0,1)
    (1,2), (1,1)

    (-2,-2), (-1,-1)    // x positions (only possible in part 2)
    (2,-2), (1,-1)
    (-2,2), (-1,1)
    (2,2), (1,1)
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
    // same as before except we should continue pairs of movements
    // the instruction only affects the last (or first?) entry, then each subsequent adjust is based on the rules as before
    // the final position is the tail

    let start = Array.create 10 (0, 0)
    ((Set [ (0,0) ], start), readEmbeddedRaw "day09")
    ||> Seq.fold (fun (visited, rope) instruction -> 
        let (dx, dy), mag = 
            let p = split " " instruction
            dirMap[p[0]], Int32.Parse p[1]
        ((visited, rope), [1..mag])
        ||> Seq.fold (fun (visited, rope) _ ->
            let nextRope, _ =
                (None, rope) 
                ||> Array.mapFold (fun prev (x, y) ->
                    match prev with
                    | None -> (x + dx, y + dy), Some (x + dx, y + dy)
                    | Some (px, py) ->
                        let x, y =
                            match Map.tryFind (px - x, py - y) adjust with
                            | None -> x, y
                            | Some (dx, dy) -> x + dx, y + dy
                        (x, y), Some (x, y) )
            Set.add nextRope[9] visited, nextRope))
    |> fst |> Seq.length