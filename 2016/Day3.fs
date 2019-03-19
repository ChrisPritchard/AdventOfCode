(*
--- Day 3: Squares With Three Sides ---

Now that you can think clearly, you move deeper into the labyrinth of hallways and office furniture that makes up this part of Easter Bunny HQ. This must be a graphic design department; the walls are covered in specifications for triangles.

Or are they?

The design document gives the side lengths of each triangle it describes, but... 5 10 25? Some of these aren't triangles. You can't help but mark the impossible ones.

In a valid triangle, the sum of any two sides must be larger than the remaining side. For example, the "triangle" given above is impossible, because 5 + 10 is not larger than 25.

In your puzzle input, how many of the listed triangles are possible?
*)

(*
--- Part Two ---

Now that you've helpfully marked up their design documents, it occurs to you that triangles are specified in groups of three vertically. Each set of three numbers in a column specifies a triangle. Rows are unrelated.

For example, given the following specification, numbers with the same hundreds digit would be part of the same triangle:

101 301 501
102 302 502
103 303 503
201 401 601
202 402 602
203 403 603

In your puzzle input, and instead reading by columns, how many of the listed triangles are possible?

*)

module Day3

open System
open System.IO

let input = 
    File.ReadAllLines "Day3-input.txt" 
    |> Array.filter (String.IsNullOrWhiteSpace >> not) 
    |> Array.map (fun line -> 
        line.Split(' ', StringSplitOptions.RemoveEmptyEntries) |> Array.map int)

let part1 () = 
    input 
    |> Array.map Array.sort 
    |> Array.filter (fun sides -> sides.[0] + sides.[1] > sides.[2]) 
    |> Array.length

let part2 () =
    input 
    |> Array.chunkBySize 3 
    |> Array.collect (fun rows -> 
        [|
            for c in 0..2 do
                yield
                    [|
                        for r in 0..2 do yield rows.[r].[c]
                    |]
        |])
    |> Array.map Array.sort
    |> Array.filter (fun sides -> sides.[0] + sides.[1] > sides.[2]) 
    |> Array.length