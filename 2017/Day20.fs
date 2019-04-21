(*
--- Day 20: Particle Swarm ---

Suddenly, the GPU contacts you, asking for help. Someone has asked it to simulate too many particles, and it won't be able to finish them all in time to render the next frame at this rate.

It transmits to you a buffer (your puzzle input) listing each particle in order (starting with particle 0, then particle 1, particle 2, and so on). For each particle, it provides the X, Y, and Z coordinates for the particle's position (p), velocity (v), and acceleration (a), each in the format <X,Y,Z>.

Each tick, all particles are updated simultaneously. A particle's properties are updated in the following order:

    Increase the X velocity by the X acceleration.
    Increase the Y velocity by the Y acceleration.
    Increase the Z velocity by the Z acceleration.
    Increase the X position by the X velocity.
    Increase the Y position by the Y velocity.
    Increase the Z position by the Z velocity.

Because of seemingly tenuous rationale involving z-buffering, the GPU would like to know which particle will stay closest to position <0,0,0> in the long term. Measure this using the Manhattan distance, which in this situation is simply the sum of the absolute values of a particle's X, Y, and Z position.

For example, suppose you are only given two particles, both of which stay entirely on the X-axis (for simplicity). Drawing the current states of particles 0 and 1 (in that order) with an adjacent a number line and diagram of current X positions (marked in parentheses), the following would take place:

p=< 3,0,0>, v=< 2,0,0>, a=<-1,0,0>    -4 -3 -2 -1  0  1  2  3  4
p=< 4,0,0>, v=< 0,0,0>, a=<-2,0,0>                         (0)(1)

p=< 4,0,0>, v=< 1,0,0>, a=<-1,0,0>    -4 -3 -2 -1  0  1  2  3  4
p=< 2,0,0>, v=<-2,0,0>, a=<-2,0,0>                      (1)   (0)

p=< 4,0,0>, v=< 0,0,0>, a=<-1,0,0>    -4 -3 -2 -1  0  1  2  3  4
p=<-2,0,0>, v=<-4,0,0>, a=<-2,0,0>          (1)               (0)

p=< 3,0,0>, v=<-1,0,0>, a=<-1,0,0>    -4 -3 -2 -1  0  1  2  3  4
p=<-8,0,0>, v=<-6,0,0>, a=<-2,0,0>                         (0)   

At this point, particle 1 will never be closer to <0,0,0> than particle 0, and so, in the long run, particle 0 will stay closest.

Which particle will stay closest to position <0,0,0> in the long term?

--- Part Two ---

To simplify the problem further, the GPU would like to remove any particles that collide. Particles collide if their positions ever exactly match. Because particles are updated simultaneously, more than two particles can collide at the same time and place. Once particles collide, they are removed and cannot collide with anything else after that tick.

For example:

p=<-6,0,0>, v=< 3,0,0>, a=< 0,0,0>    
p=<-4,0,0>, v=< 2,0,0>, a=< 0,0,0>    -6 -5 -4 -3 -2 -1  0  1  2  3
p=<-2,0,0>, v=< 1,0,0>, a=< 0,0,0>    (0)   (1)   (2)            (3)
p=< 3,0,0>, v=<-1,0,0>, a=< 0,0,0>

p=<-3,0,0>, v=< 3,0,0>, a=< 0,0,0>    
p=<-2,0,0>, v=< 2,0,0>, a=< 0,0,0>    -6 -5 -4 -3 -2 -1  0  1  2  3
p=<-1,0,0>, v=< 1,0,0>, a=< 0,0,0>             (0)(1)(2)      (3)   
p=< 2,0,0>, v=<-1,0,0>, a=< 0,0,0>

p=< 0,0,0>, v=< 3,0,0>, a=< 0,0,0>    
p=< 0,0,0>, v=< 2,0,0>, a=< 0,0,0>    -6 -5 -4 -3 -2 -1  0  1  2  3
p=< 0,0,0>, v=< 1,0,0>, a=< 0,0,0>                       X (3)      
p=< 1,0,0>, v=<-1,0,0>, a=< 0,0,0>

------destroyed by collision------    
------destroyed by collision------    -6 -5 -4 -3 -2 -1  0  1  2  3
------destroyed by collision------                      (3)         
p=< 0,0,0>, v=<-1,0,0>, a=< 0,0,0>

In this example, particles 0, 1, and 2 are simultaneously destroyed at the time and place marked X. On the next tick, particle 3 passes through unharmed.

How many particles are left after all collisions are resolved?
*)

module Day20

open Common

let input = 
    System.IO.File.ReadAllLines "./inputs/day20.txt"
    |> Array.map (fun line -> 
        line |> split "p=<,>va " |> Array.map int 
        |> fun a -> ((a.[0], a.[1], a.[2]), (a.[3], a.[4], a.[5]), (a.[6], a.[7], a.[8])))

let advance ((xp, yp, zp), (xv, yv, zv), (xa, ya, za)) =
    let xv, yv, zv = xv + xa, yv + ya, zv + za
    let xp, yp, zp = xp + xv, yp + yv, zp + zv
    ((xp, yp, zp), (xv, yv, zv), (xa, ya, za))

let part1 () =

    //let particles = Array.copy input |> Array.indexed
    //let iterations = 100000
    //let simulation =
    //    particles
    //    |> Array.map (fun (index, start) ->
    //        let dists, _ =
    //            (start, [1..iterations])
    //            ||> List.mapFold advance
    //        index, List.average dists)
    //simulation |> Array.minBy snd |> fst

    // after looking at the reddit solutions...
    input 
    |> Array.indexed
    |> Array.sortBy (fun (_, ((xp, yp, zp), (xv, yv, zv), (xa, ya, za))) ->
        abs xa + abs ya + abs za, abs xv + abs yv + abs zv, abs xp + abs yp + abs zp)
    |> Array.head |> fst

let part2 () =

    let start = Array.copy input |> Array.indexed
    let iterations = 1000
    let simulation = 
        (start, [1..iterations])
        ||> List.fold (fun particles _ ->
            (Map.empty, particles) 
            ||> Array.fold (fun map (index, particle) ->
                let (np, nv, na) = advance particle
                if Map.containsKey np map then
                    Map.add np None map
                else
                    Map.add np (Some (index, (np, nv, na))) map)
            |> Map.toArray |> Array.map snd |> Array.choose id)

    simulation.Length