module Day16

open Common
open System.IO

let input = File.ReadAllText "./inputs/day16.txt"

let part1 () =

    let run (input: int[]) =
        [|0..input.Length - 1|] 
        |> Array.map (fun i ->
            let total = 
                Array.skip i input 
                |> Array.chunkBySize (i + 1) 
                |> Array.mapi (fun i a -> 
                    match i % 4 with
                    | 0 -> Array.sum a
                    | 2 -> -1 * Array.sum a
                    | _ -> 0)
                |> Array.sum
            abs (total % 10))

    let rec runner current count =
        if count = 0 then
            current |> Array.truncate 8 |> Array.map (string >> char) |> asString
        else
            runner (run current) (count - 1)
    
    let ti = input |> Seq.map (string >> int) |> Seq.toArray
    runner ti 100

let part2 () =
        
    let tic = input |> Seq.toArray |> Array.map (string >> int)
    let ti = Array.init 10000 (fun _ -> Array.copy tic) |> Array.collect id
      
    //runner ti 100
    0