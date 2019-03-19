(*
--- Day 1: No Time for a Taxicab ---

Santa's sleigh uses a very high-precision clock to guide its movements, and the clock's oscillator is regulated by stars. Unfortunately, the stars have been stolen... by the Easter Bunny. To save Christmas, Santa needs you to retrieve all fifty stars by December 25th.

Collect stars by solving puzzles. Two puzzles will be made available on each day in the advent calendar; the second puzzle is unlocked when you complete the first. Each puzzle grants one star. Good luck!

You're airdropped near Easter Bunny Headquarters in a city somewhere. "Near", unfortunately, is as close as you can get - the instructions on the Easter Bunny Recruiting Document the Elves intercepted start here, and nobody had time to work them out further.

The Document indicates that you should start at the given coordinates (where you just landed) and face North. Then, follow the provided sequence: either turn left (L) or right (R) 90 degrees, then walk forward the given number of blocks, ending at a new intersection.

There's no time to follow such ridiculous instructions on foot, though, so you take a moment and work out the destination. Given that you can only walk on the street grid of the city, how far is the shortest path to the destination?

For example:

    Following R2, L3 leaves you 2 blocks East and 3 blocks North, or 5 blocks away.
    R2, R2, R2 leaves you 2 blocks due South of your starting position, which is 2 blocks away.
    R5, L5, R5, R3 leaves you 12 blocks away.

How many blocks away is Easter Bunny HQ?
*)

(*
--- Part Two ---

Then, you notice the instructions continue on the back of the Recruiting Document. Easter Bunny HQ is actually at the first location you visit twice.

For example, if your instructions are R8, R4, R4, R8, the first location you visit twice is 4 blocks away, due East.

How many blocks away is the first location you visit twice?

*)

module Day1

open System

let input = """
L2, L5, L5, R5, L2, L4, R1, R1, L4, R2, R1, L1, L4, R1, L4, L4, R5, R3, R1, L1, R1, L5, L1, R5, L4, R2, L5, L3, L3, R3, L3, R4, R4, L2, L5, R1, R2, L2, L1, R3, R4, L193, R3, L5, R45, L1, R4, R79, L5, L5, R5, R1, L4, R3, R3, L4, R185, L5, L3, L1, R5, L2, R1, R3, R2, L3, L4, L2, R2, L3, L2, L2, L3, L5, R3, R4, L5, R1, R2, L2, R4, R3, L4, L3, L1, R3, R2, R1, R1, L3, R4, L5, R2, R1, R3, L3, L2, L2, R2, R1, R2, R3, L3, L3, R4, L4, R4, R4, R4, L3, L1, L2, R5, R2, R2, R2, L4, L3, L4, R4, L5, L4, R2, L4, L4, R4, R1, R5, L2, L4, L5, L3, L2, L4, L4, R3, L3, L4, R1, L2, R3, L2, R1, R2, R5, L4, L2, L1, L3, R2, R3, L2, L1, L5, L2, L1, R4
"""

type Dir = Left | Right
type Ort = N | E | S | W

let newOrt current dir =
    match current, dir with
    | N, Left | S, Right -> W
    | N, Right | S, Left -> E
    | W, Left | E, Right -> S
    | W, Right | E, Left -> N

let newPos ort dist (x, y) =
    match ort with
    | N -> x, y - dist
    | S -> x, y + dist
    | W -> x - dist, y
    | E -> x + dist, y

let steps = 
    input.Trim().Split([|", "|], StringSplitOptions.RemoveEmptyEntries) 
    |> Array.map (fun (s: string) -> (if s.[0] = 'L' then Left else Right), int s.[1..])

let part1 () =
    
    let (fx, fy, _) =
        ((0,0,N), steps)
        ||> Array.fold (fun (x, y, ort) (dir, dist) ->
            let newOrt = newOrt ort dir
            let (nx, ny) = newPos newOrt dist (x, y)
            nx, ny, newOrt)
    
    abs fx + abs fy

let part2 () =

    let rec walker x y ort visited target steps =
        match target with
        | None ->
            match steps with
            | [] -> x, y
            | (dir, dist)::tail ->
                let newOrt = newOrt ort dir
                let (tx, ty) = newPos newOrt dist (x, y)
                walker x y newOrt visited (Some (tx, ty)) tail
        | Some (tx, ty) ->
            let (nx, ny) = newPos ort 1 (x, y)
            if Set.contains (nx, ny) visited then nx, ny
            else
                let newVisited = Set.add (nx, ny) visited
                let newTarget = if (nx, ny) = (tx, ty) then None else target
                walker nx ny ort newVisited newTarget steps

    let fx, fy = walker 0 0 N Set.empty None (List.ofArray steps)
    abs fx + abs fy