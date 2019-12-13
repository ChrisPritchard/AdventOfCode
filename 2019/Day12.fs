module Day12

open Common
open System.IO

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

(*
Ultimately I had to go to reddit to get the hint on calculating axii independently. To be clear, the way part 2 works is:

- The x, y, and z movements of the moons effect themselves, but not each other. 
  That is, moon A x affects moon B, C and D x, but doesnt affect moon A y or any of the other ys.
  So each axis (x, y and z), can be viewed in isolation.
   
- The obvious insight for part two is to find the cycle: what number of steps result in the total state repeating.
  However, that is un-bruteforceable (at least not in anything less than a day or more) for all three axis at once.
   
- But, if you look for the cycle for each axis independently, then you can find them quickly. 
  For my data they're each in the 200k range.
  
- So the final result is to find the number that will coincide with all three cycles. 
  The count where all three cycles will repeat at the same time.
  
- This is called the least common multiple. 
  Brute forcing this is possible, but slow: multiplying the largest cycle until you find a number that mods 0 with the other two. 
  A formula that uses the greatest common denominator is much faster.
  
The final number is insanely large, proving that this would have been likely impossible using even a very powerful computer.
*)

let part2 () =

    let axis moons = 
        [|
        (moons |> Array.map (fun ((x, _, _), (vx, _, _)) -> x, vx))
        (moons |> Array.map (fun ((_, y, _), (_, vy, _)) -> y, vy))
        (moons |> Array.map (fun ((_, _, z), (_, _, vz)) -> z, vz))
        |]

    let start = axis moons

    let rec findCycles acc cnt moons =
        if Map.count acc = Array.length start then acc
        else
            let next = timeStep moons
            let curr = axis next
            let acc = 
                (acc, Array.indexed curr) 
                ||> Array.fold (fun acc (i, o) -> 
                    if acc.ContainsKey i then acc elif o = start.[i] then Map.add i (cnt + 1UL) acc else acc)
            findCycles acc (cnt + 1UL) next
    
    let vCycles = findCycles Map.empty 0UL moons |> Map.toArray |> Array.map snd |> Array.sortDescending

    // using def from https://en.wikipedia.org/wiki/Least_common_multiple
    let rec gcd a b = if b = 0UL then a else gcd b (a % b)
    vCycles |> Array.reduce (fun a b -> (a * b) / gcd a b) |> string
