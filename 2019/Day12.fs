module Day12

open Common
open System.IO
open System.Collections.Generic

let input = 
    File.ReadAllLines ("./inputs/day12.txt") 
    |> Array.map (fun line -> 
        let parts = split "<x=, yz>" line
        (int parts.[0], int parts.[1], int parts.[2]))

let moons = input |> Array.map (fun p -> p, (0, 0, 0))

let timeStep moons =
    let appliedGravity = 
        moons
        |> Array.map (fun ((x, y, z), v) ->
            let nv = 
                (v, moons) ||> Array.fold (fun (vx, vy, vz) ((ox, oy, oz), _) ->
                    (if ox > x then vx + 1 elif ox < x then vx - 1 else vx),
                    (if oy > y then vy + 1 elif oy < y then vy - 1 else vy),
                    (if oz > z then vz + 1 elif oz < z then vz - 1 else vz))
            (x, y, z), nv)
        
    appliedGravity
    |> Array.map (fun ((x, y, z), (vx, vy, vz)) ->
        (x + vx, y + vy, z + vz), (vx, vy, vz))

let energy moons = 
    moons 
    |> Array.sumBy (fun ((x, y, z), (vx, vy, vz)) -> 
        (abs x + abs y + abs z) * (abs vx + abs vy + abs vz))

let part1 () =    

    let rec runSteps rem moons =
        if rem = 0 then moons
        else
            runSteps (rem - 1) (timeStep moons)

    runSteps 1000 moons |> energy

let part2 () =
    
    let rec counter acc moons =
        let next = timeStep moons
        if next.[0] = moons.[0] then acc
        else
            counter (acc + 1UL) next

    counter 0UL moons